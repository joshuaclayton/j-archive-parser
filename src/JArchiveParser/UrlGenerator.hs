module JArchiveParser.UrlGenerator
    ( gameUrl
    , seasonUrl
    ) where

import Text.Printf
import Network.HTTP

gameUrl :: Int -> String
gameUrl gId =
    printf "http://j-archive.com/showgame.php?%s" params
  where
    params = urlEncodeVars [("game_id", show gId)]

seasonUrl :: Int -> String
seasonUrl sId =
    printf "http://j-archive.com/showseason.php?%s" params
  where
    params = urlEncodeVars [("season_id", show sId)]
