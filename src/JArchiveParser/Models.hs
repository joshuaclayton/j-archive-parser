module JArchiveParser.Models
    ( Clue, Game, Round, buildClue, buildGame, buildRound
    , Category, buildCategory
    ) where

data Clue = Clue
    { question :: String
    , answer :: String
    , value :: String
    } deriving Show

data Category = Category
    { name :: String
    } deriving Show

data Game = Game
    { id :: Int
    , url :: String
    } deriving Show

data Round = Round
    { categories :: [Category]
    , clues :: [Maybe Clue]
    } deriving Show

buildClue :: String -> String -> String -> Clue
buildClue = Clue

buildGame :: Int -> String -> Game
buildGame = Game

buildRound :: [Category] -> [Maybe Clue] -> Round
buildRound = Round

buildCategory :: String -> Category
buildCategory = Category
