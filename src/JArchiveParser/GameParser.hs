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
import JArchiveParser.UrlGenerator (gameUrl)

someFunc :: IO ()
someFunc = do
  result <- extractClues $ GameId 173
  mapM_ (putStrLn . show) result

extractClues :: GameId -> IO [Game]
extractClues gameId = do
    runX $ fromUrl url >>> extractGame url gameId
  where
    url = gameUrl gameId

extractGame :: ArrowXml a => String -> GameId -> a XmlTree Game
extractGame gameUrl gameId = proc xml -> do
  rounds <- extractRounds -< xml
  returnA -< Game gameId gameUrl rounds

extractRounds :: ArrowXml a => a XmlTree [Round]
extractRounds = proc xml -> do
    round1 <- css "#jeopardy_round" >>> extractRound Jeopardy -< xml
    round2 <- css "#double_jeopardy_round" >>> extractRound DoubleJeopardy -< xml
    returnA -< [round1, round2]

extractRound :: ArrowXml a => RoundType -> a XmlTree Round
extractRound roundType = proc xml -> do
    clues <- listA tr -< xml
    categories <- listA extractCategory -< xml
    returnA -< Round categories (cluesMergedWithCategories clues categories) roundType
  where
    tr = css "tr" >>> extractClue'
    extractClue' = listA $ css ".clue" >>> extractClue
    cluesMergedWithCategories clues categories = zipWith (<*>) (concat clues) (cycle $ fmap Just categories)

extractCategory :: ArrowXml a => a XmlTree Category
extractCategory = proc xml -> do
  name <- css ".category_name" >>> allText -< xml
  returnA -< Category name

extractClue :: ArrowXml a => a XmlTree (Maybe (Category -> Clue))
extractClue = proc xml -> do
    answer <- maybeText extractAnswer -< xml
    question <- maybeText extractQuestion -< xml
    value <- maybeText extractValue -< xml
    returnA -< Clue <$> question <*> answer <*> value

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
