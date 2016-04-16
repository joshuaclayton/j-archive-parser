{-# LANGUAGE Arrows #-}

module JArchiveParser.GameParser
    ( someFunc
    ) where

import Text.XML.HXT.Core
import Text.XML.HXT.CSS
import Data.Tree.NTree.TypeDefs
import JArchiveParser.Request
import JArchiveParser.Models
import JArchiveParser.Regex

someFunc :: IO ()
someFunc = do
  let gameUrl i = "http://j-archive.com/showgame.php?game_id=" ++ show i
  result <- extractClues $ gameUrl 173
  mapM_ (putStrLn . show) result

extractClues :: String -> IO [Clue]
extractClues url = do
  runX $ fromUrl url >>> css "#jeopardy_round .clue" >>> extractAnswer

extractAnswer :: ArrowXml a => a XmlTree Clue
extractAnswer = proc xml -> do
    answer <- clueText -< xml
    question <- questionText -< xml
    returnA -< buildClue question answer
  where
    clueText = css ".clue_text" >>> allText
    questionText = css "div" >>> hasAttr "onmouseover" >>> getAttrValue "onmouseover" >>^ answerFromMouseOver

answerFromMouseOver :: String -> String
answerFromMouseOver mouseover =
    last $ head $ matchAllSubgroups textInsideEm mouseover
  where
    textInsideEm = mkRegex "<em .*>(.*)</em>"

allText :: ArrowXml a => a (NTree XNode) String
allText =
    allTextNodes >>^ extractTextFromNodes
  where
    allTextNodes = listA $ deep isText
    extractTextFromNodes = concat . fmap f'
    f' (NTree (XText v) _) = v
