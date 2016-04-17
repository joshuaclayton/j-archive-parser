module JArchiveParser.Models
    ( Clue, Game, Round, buildClue, buildGame, buildRound
    , Category, buildCategory
    , RoundType(..)
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
    , rounds :: [Round]
    } deriving Show

data Round = Round
    { categories :: [Category]
    , clues :: [Maybe Clue]
    , roundType :: RoundType
    } deriving Show

data RoundType = Jeopardy | DoubleJeopardy | FinalJeopardy deriving (Show)

buildClue :: String -> String -> String -> Clue
buildClue = Clue

buildGame :: Int -> String -> [Round] -> Game
buildGame = Game

buildRound :: [Category] -> [Maybe Clue] -> RoundType -> Round
buildRound = Round

buildCategory :: String -> Category
buildCategory = Category
