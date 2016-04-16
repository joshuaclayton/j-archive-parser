module JArchiveParser.Request (fromUrl) where

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.HTTP

fromUrl :: String -> IOSArrow b (NTree XNode)
fromUrl url = readDocument [withValidate        no,
                            withInputEncoding   utf8,
                            withParseByMimeType yes,
                            withHTTP            [],
                            withWarnings        no] url
