{-# LANGUAGE FlexibleInstances #-}

module JArchiveParser.Model
    ( Clue(..), Game(..), Round(..), Season(..)
    , Category(..)
    , RoundType(..)
    , GameId(..)
    , SeasonId(..)
    ) where

newtype GameId = GameId Int
newtype SeasonId = SeasonId Int

data Season = Season
    { seasonId :: SeasonId
    , sUrl :: String
    , games :: [Game]
    } deriving Show

data Clue = Clue
    { question :: String
    , answer :: String
    , value :: String
    , category :: Category
    }

data Category = Category
    { name :: String
    } deriving Show

data Game = Game
    { gameId :: GameId
    , gUrl :: String
    , rounds :: [Round]
    } deriving Show

data Round = Round
    { categories :: [Category]
    , clues :: [Maybe Clue]
    , roundType :: RoundType
    }

data RoundType = Jeopardy | DoubleJeopardy | FinalJeopardy deriving (Show)

instance Show Clue where
  show (Clue question answer value category) = "* c: " ++ name category ++ "\n  v: " ++ value ++ "\n  a: " ++ answer ++ "\n  q: " ++ question

instance Show GameId where
  show (GameId gId) = show gId

instance Show SeasonId where
  show (SeasonId sId) = show sId

instance Show Round where
  show (Round categories clues roundType) = "\nRound: " ++ show roundType ++ "\n" ++ show categories ++ "\n" ++ show clues

instance {-# OVERLAPPING #-} Show [Category] where
  show xs = "\nCategories (" ++ (show . length) xs ++ "):\n\n" ++ (concat $ map (\c -> "* " ++ name c ++ "\n") xs)

instance {-# OVERLAPPING #-} Show (Maybe Clue) where
  show (Just c) = show c
  show Nothing = "* Unanswered question"

instance {-# OVERLAPPING #-} Show [(Maybe Clue)] where
  show xs = "\nClues (" ++ (show . length) xs ++ "):\n\n" ++ (concat $ map (\c -> show c ++ "\n\n") xs)
