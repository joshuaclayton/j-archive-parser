module JArchiveParser.Models
    ( Clue, Game, Round, buildClue, buildGame, buildRound
    ) where

data Clue = Clue
    { question :: String
    , answer :: String
    } deriving Show

data Game = Game
    { id :: Int
    , url :: String
    } deriving Show

data Round = Round
    { clues :: [Clue]
    } deriving Show

buildClue :: String -> String -> Clue
buildClue = Clue

buildGame :: Int -> String -> Game
buildGame = Game

buildRound :: [Clue] -> Round
buildRound = Round
