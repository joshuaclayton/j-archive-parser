module Main where

import qualified JArchiveParser.SeasonParser as SP
import qualified JArchiveParser.GameParser as GP
import JArchiveParser.Model
import JArchiveParser.Cache
import Control.Concurrent.ParallelIO

data Settings = Settings
    { seasons :: Maybe [Int]
    , limit :: Maybe Int
    }

main :: IO ()
main = do
    mapM_ downloadSeason $ seasonsToDownload
    stopGlobalPool

downloadSeason :: SeasonId -> IO ()
downloadSeason sId = do
    season <- SP.someFunc sId
    let gamesInSeason = games season
    games' <- parallel $ map extractGame $ gamesToTake gamesInSeason
    let season' = season { games = concat games' }
    cache season'
    print season'
  where
    extractGame g = do
        putStrLn $ "Processing game " ++ (show $ gameId g)
        GP.someFunc $ gameId g

gamesToTake :: [a] -> [a]
gamesToTake =
    case limit settings of
        Nothing -> id
        Just count -> take count

seasonsToDownload :: [SeasonId]
seasonsToDownload =
    case seasons settings of
        Nothing -> fmap SeasonId [1..32]
        Just seasons' -> fmap SeasonId seasons'

settings :: Settings
settings =
    Settings (Just [1]) Nothing
