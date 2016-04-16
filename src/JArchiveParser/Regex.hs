module JArchiveParser.Regex
    ( matchAllSubgroups
    , mkRegex
    ) where

import Text.Regex.Posix ((=~))

mkRegex :: String -> String
mkRegex s = s

matchAllSubgroups :: String -> String -> [[String]]
matchAllSubgroups t r =
  t =~ r :: [[String]]
