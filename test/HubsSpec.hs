{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HubsSpec where

import           Test.Hspec                hiding ( shouldBe )
import qualified Test.Hspec

import           Control.Monad.Catch       ( MonadThrow, throwM, try )
import           Control.Monad.Reader      hiding ( join )
import           UnliftIO                  ( MonadUnliftIO )

import           Control.Lens              ( at, folded, view, (^.), (^..) )
import qualified Data.Map.Strict           as Map
import           Hubs
import qualified Network.WebSockets        as WS
import qualified Network.WebSockets.Client as WS
import qualified Network.WebSockets.Stream as WS
import           UnliftIO.STM

main :: IO ()
main = hspec spec

type State = HubsState Char

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

runHubsState :: State -> AppM a -> IO a
runHubsState state app = runReaderT (runAppM app) state

runApp :: ((Hub, Hub) -> AppM a) -> IO a
runApp action = do
  channelA <- newTChanIO
  hubA <- newTVarIO $ Room channelA mempty

  echoStream <- WS.makeEchoStream
  -- conn <- WS.newClientConnection echoStream "128.0.0.1" "/chat" WS.defaultConnectionOptions []
  -- conn <- WS.newClientConnection echoStream "128.0.0.1" "/chat2" WS.defaultConnectionOptions []
  channelB <- newTChanIO
  hubB <- newTVarIO $ Room channelB (Map.fromList [("1", Client "1" undefined False), ("2", Client "2" undefined True), ("3", Client "3" undefined False)])

  state :: HubsState Char <- newTVarIO $ Map.fromList [('a', hubA), ('b', hubB)]
  let app = action (hubA, hubB)
  runHubsState state app

shouldBe a b = liftIO $ Test.Hspec.shouldBe a b

headMaybe :: [a] -> Maybe a
headMaybe []    = Nothing
headMaybe (x:_) = Just x

spec :: Spec
spec = do
  describe "broadcast" $ do
    it "should write to send channel" $
      runApp $ \(hubA, _) -> do
        broadcast 'a' True
        channel <- view #channel <$> readTVarIO hubA
        msg <- atomically $ tryReadTChan channel
        msg `shouldBe` Just "true"

    it "shouldn't do anything when there is no room in idx" $
      runApp $ \(hubA, _) -> do
        broadcast 'z' True
        channel <- view #channel <$> readTVarIO hubA
        msg <- atomically $ tryReadTChan channel
        msg `shouldBe` Nothing

  describe "connect" $ do
    it "should add user in selected room" $
      runApp $ \(hubA, _hubB) -> do
        connect "9" 'a' undefined
        room <- readTVarIO hubA
        let client = room ^. #clients . at "9"
        (view #name <$> client) `shouldBe` Just "9"
        (view #isDisconnected <$> client) `shouldBe` Just False

    it "should reconnect disconnected user" $
      runApp $ \(_hubA, hubB) -> do
        connect "2" 'b' undefined
        room <- readTVarIO hubB
        let client = room ^. #clients . at "2"
        (view #name <$> client) `shouldBe` Just "2"
        (view #isDisconnected <$> client) `shouldBe` Just False

  describe "canConnect" $ do
    it "should not be able to connect more than max" $
      runApp $ \(_hubA, _hubB) -> do
        b <- canConnect 3 "4" 'b'
        b `shouldBe` False

    it "should not be able to connect already connected user" $
      runApp $ \(_hubA, _hubB) -> do
        b <- canConnect 3 "1" 'b'
        b `shouldBe` False

    it "should be able to connect when not full" $
      runApp $ \(_hubA, _hubB) -> do
        b <- canConnect 3 "4" 'a'
        b `shouldBe` True

    it "should be able to connect when user is disconnected and room is full" $
      runApp $ \(_hubA, _hubB) -> do
        b <- canConnect 3 "2" 'b'
        b `shouldBe` True

  describe "disconnect" $ do
    it "should disconnect user in selected room" $
      runApp $ \(_, hubB) -> do
        disconnect "1" 'b'
        room <- readTVarIO hubB
        let clients = room ^.. #clients . folded . #isDisconnected
        clients `shouldBe` [True, True, False]

    it "disconnected user stays the same" $
      runApp $ \(_, hubB) -> do
        disconnect "2" 'b'
        room <- readTVarIO hubB
        let clients = room ^.. #clients . folded . #isDisconnected
        clients `shouldBe` [False, True, False]

