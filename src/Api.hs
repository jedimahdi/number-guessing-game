{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Api where

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.STM         ( throwSTM )
import           Control.Lens
    ( at, cons, elemOf, filtered, folded, ix, over, set, toListOf, traverseOf, traversed, view,
    (%~), (.~), (^.), (^..), (^?) )
import           Control.Monad                  ( forever, replicateM )
import           Control.Monad.Catch            ( MonadThrow, throwM, try )
import           Control.Monad.IO.Class
import           Control.Monad.Reader           hiding ( join )
import           Control.Monad.Trans.Except     ( ExceptT (..) )
import           Data.Aeson
import           Data.Bool                      ( bool )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy           as LBS
import           Data.CaseInsensitive           ( mk )
import           Data.Foldable                  ( traverse_ )
import           Data.Generics.Labels           ()
import           Data.Has
import           Data.Map                       ( Map )
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     ( isNothing )
import           Data.Proxy                     ( Proxy (..) )
import           Data.Text                      ( Text )
import qualified Data.Text                      as Text
import qualified Data.Text.IO                   as TIO
import           Domain
import           Error
import           GHC.Generics                   hiding ( (:*:) )
import           Game
import qualified Hubs
import           Network.Socket                 ( Socket )
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWs
import           Network.Wai.Middleware.Cors
    ( CorsResourcePolicy (..), cors, simpleCorsResourcePolicy, simpleMethods )
import qualified Network.WebSockets             as WS
import           Prelude                        hiding ( id )
import           Servant.API
import           Servant.API.WebSocket          ( WebSocket, WebSocketPending )
import           Servant.Server                 hiding ( Unauthorized )
import           System.Random
import           UnliftIO                       ( MonadUnliftIO )
import qualified UnliftIO.Async                 as Async
import           UnliftIO.Concurrent            ( forkIO )
import           UnliftIO.Exception             ( Exception, bracket, catch, catchAny, finally )
import           UnliftIO.STM

-- receiveJson :: (MonadIO m, FromJSON a) => WS.Connection -> m (Maybe a)
-- receiveJson conn = do
--   msg <- liftIO $ WS.receiveData conn
--   pure $ decodeStrict msg

sendJson :: (MonadIO m, ToJSON a) => WS.Connection -> a -> m ()
sendJson conn x = liftIO $ WS.sendTextData conn (LBS.toStrict $ encode x)

data MsgOut
  = GameStarted GameView
  | Guessed GuessResponse
  | UserJoined Username
  | UserQuit Username
  | JoinedTo [Username]
  | GameEnded Username Int
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type State = GamesState :*: Hubs.HubsState GameId

newtype AppM a
  = App { runAppM :: ReaderT State IO a }
  deriving newtype
  ( Applicative
  , Functor
  , Monad
  , MonadIO
  , MonadReader State
  , MonadThrow
  , MonadUnliftIO
  )

appToHandler :: State -> AppM a -> Handler a
appToHandler state app = Handler $ ExceptT $ try handler
  where
    handler = runReaderT (runAppM app) state `catch` (\(ex :: AppError) -> handleError ex)
    handleError ex = throwM $ toServantError ex

toServantError :: AppError -> ServerError
toServantError (ValidationError msg) = servantErrorWithText err400 msg
toServantError (Unauthorized msg)    = servantErrorWithText err401 msg
toServantError (NotFound msg)        = servantErrorWithText err404 msg
toServantError (Conflict msg)        = servantErrorWithText err409 msg

servantErrorWithText :: ServerError -> Text -> ServerError
servantErrorWithText sErr message = sErr {errBody = errorBody, errHeaders = [jsonHeaders]}
  where
    errorBody = encode $ ApiError message
    jsonHeaders = (mk "Content-Type", "application/json;charset=utf-8")

newtype ApiError
  = ApiError { message :: Text }
  deriving stock (Generic)

instance ToJSON ApiError

type API = "games" :> Get '[JSON] [GameInfo]
      :<|> "rooms" :> Get '[JSON] [Hubs.RoomView GameId]
      :<|> "games" :> Post '[JSON] GameInfo
      :<|> "game" :> "guess" :> ReqBody '[JSON] Guess :> Post '[JSON] GuessResponse
      :<|> "socket" :> Capture "code" GameId :> Capture "username" Username :> WebSocketPending

api :: Proxy API
api = Proxy

gamesInfo :: AppM [GameInfo]
gamesInfo = do
  games <- Hubs.getKeys
  pure $ GameInfo <$> games

getRooms :: AppM [Hubs.RoomView GameId]
getRooms = Hubs.getRoomViewList

randomCode :: MonadIO m => m Text
randomCode = liftIO $
  Text.pack <$> replicateM 4 (randomRIO ('A', 'Z'))

newGame :: AppM GameInfo
newGame = do
  gameId <- randomCode
  Hubs.addEmptyRoom 3 gameId
  _ <- forkIO $ do
    Hubs.roomBroadcaster gameId `catchAny` \e -> liftIO $ putStrLn $ "roomBroadcaster error: " <> show e

    Hubs.removeRoom gameId
  pure $ GameInfo gameId

userGuessNumber :: Guess -> AppM GuessResponse
userGuessNumber guess@Guess {..} = do
  maybeGame <- getGameMay gameId
  case maybeGame of
    Just Game {..} -> do
      when (players !! turn /= username) $ throwM $ ValidationError "wrong turn"
      let isCorrect = guessNumber == number
      nextTurn <- nextPlayerTurn gameId
      let response = GuessResponse {..}
      Hubs.broadcast gameId (Guessed response)
      addGuessToLog gameId guess
      when isCorrect $ do
        Hubs.broadcast gameId (GameEnded username number)
        Hubs.endBroadcast gameId

        -- Clean Up
        removeGame gameId
      pure response
    Nothing -> throwM $ ValidationError "No game found"

server :: ServerT API AppM
server = gamesInfo
    :<|> getRooms
    :<|> newGame
    :<|> userGuessNumber
    :<|> subscribeForGameEvents

-- WebSocket

subscribeForGameEvents :: GameId -> Username -> WS.PendingConnection -> AppM ()
subscribeForGameEvents gameId username connRequest = do
  Hubs.canConnect 3 username gameId >>= bool reject go
    where
      reject :: AppM ()
      reject = liftIO $ WS.rejectRequest connRequest ""

      go :: AppM ()
      go = do
        conn <- liftIO $ WS.acceptRequest connRequest
        users <- Hubs.getRoomUsers gameId
        sendJson conn (JoinedTo users)
        Hubs.broadcast gameId (UserJoined username)
        Hubs.connect username gameId conn

        gameHubIsFull <- Hubs.isFull 3 gameId
        when gameHubIsFull $ do
          game <- getGameMay gameId >>= maybe (Hubs.getRoomUsers gameId >>= startNewGame gameId) pure
          Hubs.broadcast gameId (GameStarted (toGameView game))

        Hubs.keepConnectionAlive conn username gameId

beforeMainLoopHook :: Int -> IO ()
beforeMainLoopHook port = putStrLn $ "Started listening on 127.0.0.1:" <> show port <> "."

corsMiddleware :: Wai.Middleware
corsMiddleware = cors $ const $ Just policy
  where policy = simpleCorsResourcePolicy { corsRequestHeaders = ["authorization", "content-type"], corsMethods = "PUT" : simpleMethods }

main :: IO ()
main = do
  let port = 5000
  let settings = Warp.setBeforeMainLoop (beforeMainLoopHook port) . Warp.setPort port $ Warp.defaultSettings
  hubs <- newTVarIO mempty
  games <- newTVarIO mempty
  let state = (games, hubs)
  let servantApp = serveWithContextT api EmptyContext (appToHandler state) server
  Warp.runSettings settings . corsMiddleware $ servantApp

