{-# LANGUAGE Arrows #-}

module JArchiveParser.GameParser
    ( someFunc
    ) where

import Text.XML.HXT.Core
import Text.XML.HXT.CSS
import JArchiveParser.Request
import JArchiveParser.Model
import JArchiveParser.Regex
import JArchiveParser.Arrow.Util
import JArchiveParser.UrlGenerator (gameUrl)

someFunc :: GameId -> IO Game
someFunc gId = do
    game <- extractClues gId
    return $ head game

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
    round1 <- extractJeopardy "#jeopardy_round" Jeopardy -< xml
    round2 <- extractJeopardy "#double_jeopardy_round" DoubleJeopardy -< xml
    returnA -< [round1, round2]
  where
    extractJeopardy selector roundType =
      css selector >>> extractRound roundType

extractRound :: ArrowXml a => RoundType -> a XmlTree Round
extractRound roundType = proc xml -> do
    clues <- listA tr -< xml
    categories <- listA extractCategory -< xml
    let finalClues = cluesMergedWithCategories clues categories
    returnA -< Round categories finalClues roundType
  where
    tr = css "tr" >>> extractClue'
    extractClue' = listA $ css ".clue" >>> extractClue
    cluesMergedWithCategories clues categories =
        zipWith (<*>) (concat clues) (cycle $ fmap Just categories)

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
    answerFromMouseOver mo = last $ head $ matchAllSubgroups textInsideEm mo
    textInsideEm = mkRegex "<em[^>]*>(.+)</em>"

extractValue :: ArrowXml a => a XmlTree String
extractValue =
    (css ".clue_value" >>> allText)
    `orElse`
    (css ".clue_value_daily_double" >>> allText)

maybeText :: ArrowXml a => a XmlTree String -> a XmlTree (Maybe String)
maybeText = arrToMaybe ((/=) "")
