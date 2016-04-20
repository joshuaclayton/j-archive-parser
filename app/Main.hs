module Main where

import qualified JArchiveParser.SeasonParser as SP
import qualified JArchiveParser.GameParser as GP
import JArchiveParser.Model
import JArchiveParser.Cache
import Control.Concurrent.ParallelIO

main :: IO ()
main = do
    season <- SP.someFunc $ SeasonId 1
    let gamesInSeason = games season
    mapM_ print gamesInSeason
    games' <- parallel $ map extractGame $ gamesToTake gamesInSeason
    stopGlobalPool
    let season' = season { games = games' }
    cache season'
    print season'
  where
    gamesToTake = take 2
    extractGame g = GP.someFunc $ gameId g
