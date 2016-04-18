{-# LANGUAGE Arrows #-}

module Lib
    ( someFunc
    ) where

import Text.XML.HXT.Core
import Text.XML.HXT.CSS
import JArchiveParser.Request
import JArchiveParser.Models
import JArchiveParser.Regex
import JArchiveParser.Arrow.Util
import JArchiveParser.UrlGenerator (seasonUrl)

someFunc :: IO ()
someFunc = do
  result <- extractGames $ seasonUrl $ SeasonId 1
  mapM_ (putStrLn . show) result

extractGames :: String -> IO [Game]
extractGames url = do
  runX $ fromUrl url >>> css "table td a" >>> extractGame

extractGame :: ArrowXml cat => cat XmlTree Game
extractGame = proc xml -> do
    url <- getAttrValue "href" -< xml
    let gId = last $ head $ matchAllSubgroups gameIdMatch url
    returnA -< buildGame (toGid gId) url []
  where
    toGid t = GameId ((read t) :: Int)
    gameIdMatch = mkRegex "game_id=([0-9]+)"
