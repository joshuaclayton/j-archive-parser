module Main where

import qualified JArchiveParser.SeasonParser as SP
import qualified JArchiveParser.GameParser as GP
import JArchiveParser.Model
import Control.Concurrent.ParallelIO

main :: IO ()
main = do
    gamesInSeason <- SP.someFunc $ SeasonId 1
    mapM_ (putStrLn . show) gamesInSeason
    games <- parallel $ map extractGame $ gamesToTake gamesInSeason
    stopGlobalPool
    mapM_ (putStrLn . show) games
  where
    gamesToTake = take 2
    extractGame g = GP.someFunc $ gameId g
