{-# LANGUAGE Arrows #-}

module JArchiveParser.SeasonParser
    ( someFunc
    ) where

import Text.XML.HXT.Core
import Text.XML.HXT.CSS
import JArchiveParser.Request
import JArchiveParser.Model
import JArchiveParser.Regex
import JArchiveParser.Arrow.Util
import JArchiveParser.UrlGenerator (seasonUrl)

someFunc :: SeasonId -> IO Season
someFunc sId = do
    season <- extractSeasons sId
    return $ head season

extractSeasons :: SeasonId -> IO [Season]
extractSeasons seasonId = do
    runX $ fromUrl url >>> css "table" >>> extractSeason url seasonId
  where
    url = seasonUrl seasonId

extractSeason :: ArrowXml cat => String -> SeasonId -> cat XmlTree Season
extractSeason url seasonId = proc xml -> do
    games' <- listA extractGame' -< xml
    returnA -< Season seasonId url games'
  where
    extractGame' = css "td:nth-of-type(1) a" >>> extractGame

extractGame :: ArrowXml cat => cat XmlTree Game
extractGame = proc xml -> do
    url <- getAttrValue "href" -< xml
    let gId = last $ head $ matchAllSubgroups gameIdMatch url
    returnA -< Game (toGid gId) url []
  where
    toGid t = GameId ((read t) :: Int)
    gameIdMatch = mkRegex "game_id=([0-9]+)"
