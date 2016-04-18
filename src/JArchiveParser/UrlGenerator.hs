module JArchiveParser.UrlGenerator
    ( gameUrl
    , seasonUrl
    ) where

import Text.Printf
import Network.HTTP
import JArchiveParser.Models

gameUrl :: GameId -> String
gameUrl gId =
    printf "http://j-archive.com/showgame.php?%s" params
  where
    params = urlEncodeVars [("game_id", show gId)]

seasonUrl :: SeasonId -> String
seasonUrl sId =
    printf "http://j-archive.com/showseason.php?%s" params
  where
    params = urlEncodeVars [("season", show sId)]
