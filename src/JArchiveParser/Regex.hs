{-# LANGUAGE FlexibleContexts #-}

module JArchiveParser.Regex
    ( matchAllSubgroups
    , mkRegex
    ) where

import Text.Regex.TDFA
import Data.List (unfoldr)

mkRegex :: RegexMaker Regex CompOption ExecOption String => String -> Regex
mkRegex str =
    makeRegex str

matchAllSubgroups :: Regex -> String -> [[String]]
matchAllSubgroups re =
    unfoldr f
  where
    f :: String -> Maybe ([String], String)
    f str = do
        (_, _, rest, groups) <-
            matchM re str :: Maybe (String, String, String, [String])
        return (groups, rest)
