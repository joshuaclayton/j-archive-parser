module Main where

import qualified Lib as S
import JArchiveParser.Models
import JArchiveParser.GameParser
import Control.Concurrent.ParallelIO

main :: IO ()
main = do
    gamesInSeason <- S.someFunc $ SeasonId 1
    mapM_ (putStrLn . show) gamesInSeason
    games <- parallel $ map extractGame $ gamesToTake gamesInSeason
    mapM_ (putStrLn . show) games
    stopGlobalPool
  where
    gamesToTake = take 2
    extractGame g = someFunc $ gameId g
