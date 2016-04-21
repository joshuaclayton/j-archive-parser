{-# LANGUAGE OverloadedStrings #-}

module JArchiveParser.Cache
    ( cache
    ) where

import Data.Csv
import Data.Maybe (catMaybes)
import JArchiveParser.Model
import qualified Data.ByteString.Lazy as BS

data FlattenedClue = FlattenedClue
    { fcSId :: SeasonId
    , fcGId :: GameId
    , fcRoundType :: RoundType
    , fcQuestion :: String
    , fcAnswer :: String
    , fcValue :: String
    , fcCategory :: Category
    }

cache :: Season -> IO ()
cache season =
    writeToCache file $ seasonToFlattenedClues season
  where
    file =  "season" ++ show seasonId' ++ ".csv"
    seasonId' = seasonId season

seasonToFlattenedClues :: Season -> [FlattenedClue]
seasonToFlattenedClues season = do
    game <- games season
    round <- rounds game
    clue <- catMaybes $ clues round
    return $ FlattenedClue
        (seasonId season)
        (gameId game)
        (roundType round)
        (question clue)
        (answer clue)
        (value clue)
        (category clue)

writeToCache :: String -> [FlattenedClue] -> IO ()
writeToCache cacheFile = BS.writeFile cacheFile . encodeDefaultOrderedByName

instance ToNamedRecord FlattenedClue where
    toNamedRecord (FlattenedClue fcSId fcGId fcRoundType fcQuestion fcAnswer fcValue fcCategory) =
        namedRecord [ "season id" .= fcSId
                    , "game id" .= fcGId
                    , "round" .= fcRoundType
                    , "category" .= name fcCategory
                    , "value" .= fcValue
                    , "answer" .= fcAnswer
                    , "question" .= fcQuestion
                    ]

instance DefaultOrdered FlattenedClue where
    headerOrder _ = header ["season id"
                           , "game id"
                           , "round"
                           , "category"
                           , "value"
                           , "answer"
                           , "question"
                           ]

instance ToField SeasonId where
    toField (SeasonId sId) = toField sId

instance ToField GameId where
    toField (GameId gId) = toField gId

instance ToField RoundType where
    toField Jeopardy = "Jeopardy"
    toField DoubleJeopardy = "Double Jeopardy"
