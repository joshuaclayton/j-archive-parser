module JArchiveParser.Arrow.Util
    ( allText
    , arrToMaybe
    ) where

import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.Core

arrToMaybe :: ArrowXml a => (b -> Bool) -> a XmlTree b -> a XmlTree (Maybe b)
arrToMaybe f x =
  (x >>> isA f >>> arr Just) `orElse` (constA Nothing)

allText :: ArrowXml a => a (NTree XNode) String
allText =
    allTextNodes >>^ extractTextFromNodes >>> arr concat
  where
    allTextNodes = listA $ deep isText
    extractTextFromNodes = fmap f'
    f' (NTree (XText v) _) = v
