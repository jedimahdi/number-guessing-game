{-# OPTIONS_GHC -Wno-deprecations #-}
module Hubs where

import           Common
import           Control.Lens         (_1, _2, at, cons, elemOf, filtered,
                                       folded, itoList, ix, over, sans, set,
                                       toListOf, traverseOf, traversed, view,
                                       (%~), (.~), (^.), (^..), (^?))
import           Control.Monad.Catch  (MonadThrow, throwM)
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable        (traverse_)
import           Data.Has             (Has, getter)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (isNothing)
import           Data.Text            (Text)
import qualified Data.Text.IO         as TIO
import           Domain
import           Error
import           GHC.Generics         (Generic)
import           Network.WebSockets   (ConnectionException (..))
import qualified Network.WebSockets   as WS
import           Prelude              hiding (max)
import           UnliftIO             (MonadUnliftIO)
import           UnliftIO.Concurrent  (forkIO)
import           UnliftIO.Exception   (catch)
import           UnliftIO.STM

data Room = Room { channel    :: TChan ByteString
                 , clients    :: Map Username Client
                 , maxClients :: Int
                 }
  deriving stock (Generic)


instance Show Room where
  show Room {..} = "Room " <> show clients

data RoomView k = RoomView { maxClients   :: Int
                           , clientsCount :: Int
                           , roomId       :: k
                           }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

type Hub = TVar Room

type Hubs k = Map k Hub

data Client = Client { name           :: Username
                     , socket         :: WS.Connection
                     , isDisconnected :: Bool
                     }
  deriving stock (Generic)

instance Show Client where
  show Client {..} = "Client " <> show name <> " isDisconnected " <> show isDisconnected

type HubsState k = TVar (Hubs k)

type HubOperation k r m = (MonadReader r m, MonadIO m, MonadUnliftIO m, Ord k, Has (HubsState k) r)

mkEmptyRoom :: MonadIO m => Int -> m Room
mkEmptyRoom max = do
  channel <- newTChanIO
  pure $ Room channel mempty max

roomBroadcaster :: HubOperation k r m => k -> m ()
roomBroadcaster roomId = do
  withRoomDo roomId (pure ()) $ \(room, roomState) -> do
      msg <- atomically $ readTChan (room ^. #channel)
      unless (msg == "endBroadcast") $ do
        clients <- toListOf (#clients . folded . filtered (not . view #isDisconnected) . #socket) <$> readTVarIO roomState
        liftIO $ print msg
        traverse_ (liftIO . flip WS.sendTextData msg) clients
        roomBroadcaster roomId

forkRoomBroadcaster :: HubOperation k r m => k -> m ()
forkRoomBroadcaster roomId = do
  _ <- forkIO $ do
    roomBroadcaster roomId
    removeRoom roomId
  pure ()

existsAt :: HubOperation k r m => k -> m Bool
existsAt roomId =
  withRoomDo roomId (pure False) $ const (pure True)

isFull :: HubOperation k r m => Int -> k -> m Bool
isFull max roomId =
  withRoomDo roomId (pure False) $ \(room, _) -> do
    let clients = room ^. #clients
    pure $ length clients == max

canConnect :: forall k r m. HubOperation k r m => Int -> Username -> k -> m Bool
canConnect max username roomId =
  withRoomDo roomId (pure False) go
  where
    go :: (Room, Hub) -> m Bool
    go (room, _) | length (room ^. #clients) < max = pure True
                 | elemOf (folded . #name) username (room ^.. #clients . folded . filtered (view #isDisconnected)) = pure True
                 | otherwise = pure False

connect :: HubOperation k r m => Username -> k -> WS.Connection -> m ()
connect username roomId conn =
  withRoomDo roomId (pure ()) $ \(_, roomState) ->
    atomically $ modifyTVar roomState (set (#clients . at username) (Just (Client username conn False)))

disconnect :: HubOperation k r m => Username -> k -> m ()
disconnect username roomId =
  withRoomDo roomId (pure ()) $ \(_, roomState) ->
    atomically $ modifyTVar roomState (over (#clients . ix username) (\client -> client {isDisconnected = True}))

removeConnection :: HubOperation k r m => Username -> k -> m ()
removeConnection username roomId =
  withRoomDo roomId (pure ()) $ \(_, roomState) ->
    atomically $ modifyTVar roomState (over #clients (sans username))

keepConnectionAlive :: forall k r m. HubOperation k r m => WS.Connection -> Username -> k -> m ()
keepConnectionAlive conn username roomId = do
  liftIO $ WS.forkPingThread conn 30
  keepListening `catch` handleClosed
  where
    keepListening :: m ()
    keepListening = liftIO $ forever $ void (WS.receiveData conn :: IO Text)

    handleClosed :: ConnectionException -> m ()
    handleClosed (CloseRequest _ _) = disconnect username roomId
    handleClosed ConnectionClosed   = disconnect username roomId
    handleClosed _                  = pure ()

addEmptyRoom :: HubOperation k r m => Int -> k -> m ()
addEmptyRoom maxClients roomId =
  withHubsDo $ \(_, hState) -> do
    room <- mkEmptyRoom maxClients
    roomVar <- newTVarIO room
    atomically $ modifyTVar hState (set (at roomId) (Just roomVar))

removeRoom :: HubOperation k r m => k -> m ()
removeRoom roomId =
  withHubsDo $ \(_, hState) -> do
    atomically $ modifyTVar hState (set (at roomId) Nothing)

getKeys :: HubOperation k r m => m [k]
getKeys = do
  withHubsDo $ \(hubs, _) -> do
    pure $ map fst $ itoList hubs

toRoomView :: HubOperation k r m => (k, Hub) -> m (RoomView k)
toRoomView (roomId, hub) = do
  room <- readTVarIO hub
  let clientsCount = length (room ^. #clients)
  let maxClients = room ^. #maxClients
  pure $ RoomView {..}

getRoomViewList :: HubOperation k r m => m [RoomView k]
getRoomViewList = do
  withHubsDo $ \(hubs, _) -> do
    traverse toRoomView . itoList $ hubs

getRoomUsers :: HubOperation k r m => k -> m [Username]
getRoomUsers roomId =
  withRoomDo roomId (pure []) $ \(room, _) -> do
    pure $ room ^.. #clients . folded . #name

getRoomClients :: HubOperation k r m => k -> m (Maybe [Client])
getRoomClients roomId =
  withRoomDo roomId (pure Nothing) $ \(room, _) -> do
    pure . pure $ room ^.. #clients . folded

broadcast :: (HubOperation k r m, ToJSON a) => k -> a -> m ()
broadcast roomId message =
  withRoomDo roomId (pure ()) $ \(room, _) ->
    atomically $ writeTChan (room ^. #channel) (LBS.toStrict $ encode message)

endBroadcast :: HubOperation k r m => k -> m ()
endBroadcast roomId =
  withRoomDo roomId (pure ()) $ \(room, _) ->
    atomically $ writeTChan (room ^. #channel) "endBroadcast"

withHubsDo :: HubOperation k r m => ((Hubs k, HubsState k) -> m a) -> m a
withHubsDo action = do
  hubsState <- asks getter
  hubsMap <- liftIO . readTVarIO $ hubsState
  action (hubsMap, hubsState)

withRoomDo :: forall k r m a. HubOperation k r m => k -> m a -> ((Room, Hub) -> m a) -> m a
withRoomDo roomId notFoundAction action = do
  hubsState <- asks (getter @(HubsState k))
  hubsMap <- liftIO . readTVarIO $ hubsState
  case hubsMap ^. at roomId of
    Just roomState -> do
      room <- readTVarIO roomState
      action (room, roomState)
    Nothing   -> notFoundAction
