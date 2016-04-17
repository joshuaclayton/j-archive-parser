{-# LANGUAGE Arrows #-}

module JArchiveParser.GameParser
    ( someFunc
    ) where

import Text.XML.HXT.Core
import Text.XML.HXT.CSS
import JArchiveParser.Request
import JArchiveParser.Models
import JArchiveParser.Regex
import JArchiveParser.Arrow.Util

someFunc :: IO ()
someFunc = do
  let gameUrl i = "http://j-archive.com/showgame.php?game_id=" ++ show i
  result <- extractClues $ gameUrl 173
  mapM_ (putStrLn . show) result

extractClues :: String -> IO [Round]
extractClues url = do
  runX $ fromUrl url >>> css "#jeopardy_round" >>> extractRound

extractRound :: ArrowXml a => a XmlTree Round
extractRound = proc xml -> do
    clues <- listA tr -< xml
    categories <- listA extractCategory -< xml
    returnA -< buildRound categories (concat clues)
  where
    tr = css "tr" >>> extractClue'
    extractClue' = listA $ css ".clue" >>> extractClue

extractCategory :: ArrowXml a => a XmlTree Category
extractCategory = proc xml -> do
  name <- css ".category_name" >>> allText -< xml
  returnA -< buildCategory name

extractClue :: ArrowXml a => a XmlTree (Maybe Clue)
extractClue = proc xml -> do
    answer <- maybeText extractAnswer -< xml
    question <- maybeText extractQuestion -< xml
    value <- maybeText extractValue -< xml
    returnA -< buildClue <$> question <*> answer <*> value

extractAnswer :: ArrowXml a => a XmlTree String
extractAnswer =
    css ".clue_text" >>> allText

extractQuestion :: ArrowXml a => a XmlTree String
extractQuestion =
    css "div" >>> getAttrValue "onmouseover" >>^ answerFromMouseOver
  where
    answerFromMouseOver mouseover = last $ head $ matchAllSubgroups textInsideEm mouseover
    textInsideEm = mkRegex "<em[^>]*>(.+)</em>"

extractValue :: ArrowXml a => a XmlTree String
extractValue =
    (css ".clue_value" >>> allText) `orElse` (css ".clue_value_daily_double" >>> allText)

maybeText :: ArrowXml a => a XmlTree String -> a XmlTree (Maybe String)
maybeText = arrToMaybe ((/=) "")
