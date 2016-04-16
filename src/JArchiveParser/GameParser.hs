{-# LANGUAGE Arrows #-}

module JArchiveParser.GameParser
    ( someFunc
    ) where

import Text.XML.HXT.Core
import Text.XML.HXT.CSS
import Data.Tree.NTree.TypeDefs
import Data.Maybe
import JArchiveParser.Request
import JArchiveParser.Models
import JArchiveParser.Regex

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
    returnA -< buildRound categories (catMaybes $ concat clues)
  where
    tr = css "tr" >>> extractClue'
    extractClue' = listA $ css ".clue" >>> extractClue

extractCategory :: ArrowXml a => a XmlTree Category
extractCategory = proc xml -> do
  name <- css ".category_name" >>> allText -< xml
  returnA -< buildCategory name

extractClue :: ArrowXml a => a XmlTree (Maybe Clue)
extractClue = proc xml -> do
    answer <- clueText -< xml
    question <- maybeQuestion -< xml
    returnA -< question >>= \q -> return $ buildClue q answer
  where
    clueText = css ".clue_text" >>> allText
    maybeQuestion = arrToMaybe ((/=) "") questionText
    questionText = css "div" >>> retrieveAttribute "onmouseover" >>^ answerFromMouseOver

arrToMaybe :: ArrowXml a => (b -> Bool) -> a XmlTree b -> a XmlTree (Maybe b)
arrToMaybe f x =
  (x >>> isA f >>> arr Just) `orElse` (constA Nothing)

answerFromMouseOver :: String -> String
answerFromMouseOver mouseover =
    last $ head $ matchAllSubgroups textInsideEm mouseover
  where
    textInsideEm = mkRegex "<em .*>(.*)</em>"

retrieveAttribute :: ArrowXml a => String -> a XmlTree String
retrieveAttribute a =
  hasAttr a >>> getAttrValue a

allText :: ArrowXml a => a (NTree XNode) String
allText =
    allTextNodes >>^ extractTextFromNodes >>> arr concat
  where
    allTextNodes = listA $ deep isText
    extractTextFromNodes = fmap f'
    f' (NTree (XText v) _) = v
