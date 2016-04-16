module JArchiveParser.Models
    ( Clue, Game, buildClue, buildGame
    ) where

data Clue = Clue
    { question :: String
    , answer :: String
    } deriving Show

data Game = Game
    { id :: Int
    , url :: String
    } deriving Show

buildClue :: String -> String -> Clue
buildClue = Clue

buildGame :: Int -> String -> Game
buildGame = Game
