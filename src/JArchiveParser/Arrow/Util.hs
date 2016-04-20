module JArchiveParser.Arrow.Util
    ( allText
    , arrToMaybe
    ) where

import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.Core
import Text.XML.HXT.CSS

arrToMaybe :: ArrowXml a => (b -> Bool) -> a XmlTree b -> a XmlTree (Maybe b)
arrToMaybe p x =
    (x >>> isA p >>> justA) `orElse` nothingA
  where
    justA = arr Just
    nothingA = constA Nothing

allText :: ArrowXml a => a (NTree XNode) String
allText =
    allTextNodes >>^ extractTextFromNodes >>> arr concat
  where
    allTextNodes = listA $ deep isText
    extractTextFromNodes = fmap f'
    f' (NTree (XText v) _) = v

textAtSelector :: ArrowXml cat => String -> cat XmlTree String
textAtSelector selector =
    css selector >>> getChildren >>> getText
