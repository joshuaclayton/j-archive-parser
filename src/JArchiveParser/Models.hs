{-# LANGUAGE FlexibleInstances #-}

module JArchiveParser.Models
    ( Clue, Game, Round, buildClue, buildGame, buildRound
    , Category, buildCategory
    , RoundType(..)
    ) where

data Clue = Clue
    { question :: String
    , answer :: String
    , value :: String
    }

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
    }

data RoundType = Jeopardy | DoubleJeopardy | FinalJeopardy deriving (Show)

buildClue :: String -> String -> String -> Clue
buildClue = Clue

buildGame :: Int -> String -> [Round] -> Game
buildGame = Game

buildRound :: [Category] -> [Maybe Clue] -> RoundType -> Round
buildRound = Round

buildCategory :: String -> Category
buildCategory = Category

instance Show Clue where
  show (Clue question answer value) = "* v: " ++ value ++ "\n  a: " ++ answer ++ "\n  q: " ++ question

instance Show Round where
  show (Round categories clues roundType) = "\nRound: " ++ show roundType ++ "\n" ++ show categories ++ "\n" ++ show clues

instance {-# OVERLAPPING #-} Show [Category] where
  show xs = "\nCategories (" ++ (show . length) xs ++ "):\n\n" ++ (concat $ map (\c -> "* " ++ name c ++ "\n") xs)

instance {-# OVERLAPPING #-} Show (Maybe Clue) where
  show (Just c) = show c
  show Nothing = "* Unanswered question"

instance {-# OVERLAPPING #-} Show [(Maybe Clue)] where
  show xs = "\nClues (" ++ (show . length) xs ++ "):\n\n" ++ (concat $ map (\c -> show c ++ "\n\n") xs)
