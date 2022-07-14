module Domain where

import           Data.Aeson
import           Data.Generics.Labels ()
import           Data.Text            (Text)
import           GHC.Generics
import           Prelude              hiding (id)

type Username = Text

type GameId = Text

data Game = Game { id          :: GameId
                 , players     :: [Username]
                 , isOver      :: Bool
                 , winner      :: Maybe Username
                 , guessNumber :: Int
                 , guessesLog  :: [Guess]
                 , turn        :: Int
                 }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data GameView = GameView { id         :: GameId
                         , players    :: [Username]
                         , turn       :: Username
                         , guessesLog :: [Guess]
                         }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

toGameView :: Game -> GameView
toGameView Game {..}
  = GameView { id
             , players
             , turn = players !! turn
             , guessesLog = guessesLog
             }

data GameInfo = GameInfo { gameId :: GameId
                         }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Guess = Guess { number   :: Int
                   , username :: Username
                   , gameId   :: GameId
                   }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data GuessResponse = GuessResponse { isCorrect :: Bool
                                   , number    :: Int
                                   , username  :: Username
                                   , nextTurn  :: Username
                                   }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

