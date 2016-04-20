{-# LANGUAGE FlexibleInstances #-}

module JArchiveParser.Model
    ( Clue (..)
    , Game (..)
    , Round (..)
    , Season (..)
    , Category (..)
    , RoundType (..)
    , GameId (..)
    , SeasonId (..)
    ) where

import qualified Data.List as L

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
    show (Clue question answer value category) =
        concat $ L.intersperse "\n" lines
      where
        lines = [categoryLine, valueLine, answerLine, questionLine]
        categoryLine = "* c:" ++ name category
        valueLine    = "  v:" ++ value
        answerLine   = "  a:" ++ answer
        questionLine = "  a:" ++ question

instance Show GameId where
    show (GameId gId) = show gId

instance Show SeasonId where
    show (SeasonId sId) = show sId

instance Show Round where
    show (Round categories clues roundType) =
        "\nRound: " ++ (concat $ L.intersperse "\n" lines)
      where
        lines = [show roundType, show categories, show clues]

instance {-# OVERLAPPING #-} Show [Category] where
    show xs =
        "\nCategories (" ++ categoryCount ++ "):\n\n" ++ categoryList
      where
        categoryCount = (show . length) xs
        categoryList =
            concat $ map (\c -> "* " ++ name c ++ "\n") xs

instance {-# OVERLAPPING #-} Show (Maybe Clue) where
    show (Just c) = show c
    show Nothing = "* Unanswered question"

instance {-# OVERLAPPING #-} Show [(Maybe Clue)] where
    show xs =
        "\nClues (" ++ cluesLength ++ "):\n\n" ++ cluesList
      where
        cluesLength = (show . length) xs
        cluesList =
            concat $ map (\c -> show c ++ "\n\n") xs
