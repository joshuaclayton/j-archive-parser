module Main where

import qualified JArchiveParser.SeasonParser as SP
import qualified JArchiveParser.GameParser as GP
import JArchiveParser.Model
import Control.Concurrent.ParallelIO

main :: IO ()
main = do
    season <- SP.someFunc $ SeasonId 1
    let gamesInSeason = games season
    mapM_ print gamesInSeason
    games' <- parallel $ map extractGame $ gamesToTake gamesInSeason
    stopGlobalPool
    mapM_ print games'
  where
    gamesToTake = take 2
    extractGame g = GP.someFunc $ gameId g
