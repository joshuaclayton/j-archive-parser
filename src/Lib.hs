{-# LANGUAGE Arrows #-}

module Lib
    ( someFunc
    ) where

import Text.XML.HXT.Core
import Text.XML.HXT.CSS
import JArchiveParser.Request
import JArchiveParser.Models
import JArchiveParser.Regex

someFunc :: IO ()
someFunc = do
  let seasonUrl i = "http://j-archive.com/showseason.php?season=" ++ show i
  result <- extractGames $ seasonUrl 1
  mapM_ (putStrLn . show) result

extractGames :: String -> IO [Game]
extractGames url = do
  runX $ fromUrl url >>> css "table td a" >>> extractGame

extractGame :: ArrowXml cat => cat XmlTree Game
extractGame = proc xml -> do
    url <- getAttrValue "href" -< xml
    let gId = last $ head $ url `matchAllSubgroups` gameIdMatch
    returnA -< buildGame (toInt gId) url
  where
    toInt t = ((read t) :: Int)
    gameIdMatch = mkRegex "game_id=([0-9]+)"

textAtSelector :: ArrowXml cat => String -> cat XmlTree String
textAtSelector selector =
  css selector >>> getChildren >>> getText
