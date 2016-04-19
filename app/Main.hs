module Main where

import qualified Lib as S
import JArchiveParser.Model
import JArchiveParser.GameParser
import Control.Concurrent.ParallelIO

main :: IO ()
main = do
    gamesInSeason <- S.someFunc $ SeasonId 1
    mapM_ (putStrLn . show) gamesInSeason
    games <- parallel $ map extractGame $ gamesToTake gamesInSeason
    stopGlobalPool
    mapM_ (putStrLn . show) games
  where
    gamesToTake = take 2
    extractGame g = someFunc $ gameId g
