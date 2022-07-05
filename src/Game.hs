module Game where

import           Common
import           Control.Lens
    ( at, cons, filtered, ix, over, set, toListOf, traverseOf, traversed, view, (%~), (.~), (^.),
    (^..), (^?) )
import           Control.Monad.Catch  ( MonadThrow, throwM )
import           Control.Monad.Reader
import           Data.Has             ( Has, getter )
import           Data.Map             ( Map )
import           Data.Maybe           ( isNothing )
import           Domain
import           Error
import           Prelude              hiding ( id )
import           System.Random
import           UnliftIO.STM

type GamesState = TVar (Map GameId Game)

type GameOperation r m = (MonadReader r m, Has GamesState r, MonadIO m, MonadThrow m)

startNewGame :: GameOperation r m => GameId -> [Username] -> m Game
startNewGame gameId players = do
  -- checkBool (length players /= maxPlayers) $ ValidationError "Game is not full."
  randomNumber <- randomRIO (0 :: Int, 10)
  playerIndex <- randomRIO (0 :: Int, length players - 1)
  let game = Game gameId players False Nothing randomNumber [] playerIndex
  games <- asks (getter @GamesState)
  atomically $ modifyTVar games (set (at gameId) (Just game))
  pure game

nextPlayerTurn :: GameOperation r m => GameId -> m Username
nextPlayerTurn gameId = do
  gamesState <- asks (getter @GamesState)
  gameMay <- getGameFromState gameId gamesState
  case gameMay of
    Just Game {..} -> do
      let playersCount = length players
      let nextTurnIndex = (turn + 1) `mod` playersCount
      atomically $ modifyTVar gamesState (set (ix gameId . #turn) nextTurnIndex)
      pure $ players !! nextTurnIndex
    Nothing -> throwM $ ValidationError "Game not found."

addGuessToLog :: GameOperation r m => GameId -> Guess -> m ()
addGuessToLog gameId guess = do
  games <- asks (getter @GamesState)
  atomically $ modifyTVar games (over (ix gameId . #guessesLog) (guess :))

getGameFromState :: GameOperation r m => GameId -> GamesState -> m (Maybe Game)
getGameFromState searchedId gamesState = view (at searchedId) <$> readTVarIO gamesState

getGameMay :: GameOperation r m => GameId -> m (Maybe Game)
getGameMay searchedId = do
  games <- asks getter
  getGameFromState searchedId games

getGame :: GameOperation r m => GameId -> m Game
getGame searchedId = do
  gameMay <- getGameMay searchedId
  checkMaybe gameMay $ NotFound $ "Game with id " <> searchedId <> " was not found."

addGame :: GameOperation r m => Game -> m ()
addGame game = do
  let gameId = game ^. #id
  gamesState <- asks (getter @GamesState)
  gameMay <- getGameFromState gameId gamesState
  checkBool (isNothing gameMay) $ Conflict "Game already exists."
  atomically $ modifyTVar gamesState (set (at gameId) (Just game))

removeGame :: GameOperation r m => GameId -> m ()
removeGame gameId = do
  gamesState <- asks (getter @GamesState)
  atomically $ modifyTVar gamesState (set (at gameId) Nothing)
