module Main where

import qualified JArchiveParser.SeasonParser as SP
import qualified JArchiveParser.GameParser as GP
import JArchiveParser.Model
import JArchiveParser.Cache
import Control.Concurrent.ParallelIO
import Control.Monad (void)

data ExecutionProcessor a = ExecutionProcessor
    { handleEach :: [IO a] -> IO [a]
    , handleEnd :: IO ()
    }

data Settings = Settings
    { seasons :: Maybe [Int]
    , limit :: Maybe Int
    , seasonProcessor :: (Season -> IO ())
    , executionProcessor :: ExecutionProcessor [Game]
    }

main :: IO ()
main = do
    mapM_ (downloadSeason defaultSettings) $ seasonsToDownload defaultSettings
    handleEnd $ executionProcessor defaultSettings

downloadSeason :: Settings -> SeasonId -> IO ()
downloadSeason settings sId = do
    season <- SP.someFunc sId
    games' <- extractGames $ games season
    (seasonProcessor settings) season { games = concat games' }
  where
    extractGames gs =
        handleEach' $ map extractGame $ (gamesToTake settings) gs
    extractGame g = do
        putStrLn $ "Processing game " ++ (show $ gameId g)
        GP.someFunc $ gameId g
    handleEach' =
        (handleEach $ executionProcessor settings)

gamesToTake :: Settings -> [a] -> [a]
gamesToTake settings =
    case limit settings of
        Nothing -> id
        Just count -> take count

seasonsToDownload :: Settings -> [SeasonId]
seasonsToDownload settings =
    case seasons settings of
        Nothing -> fmap SeasonId [1..32]
        Just seasons' -> fmap SeasonId seasons'

processSeason :: Season -> IO ()
processSeason s = do
    cache s
    print s

sequentialProcessor :: ExecutionProcessor [Game]
sequentialProcessor =
    ExecutionProcessor sequence handlerEnd
  where
    handlerEnd = return (void putStrLn "") :: IO ()

parallelProcessor :: ExecutionProcessor [Game]
parallelProcessor =
    ExecutionProcessor parallel stopGlobalPool

defaultSettings :: Settings
defaultSettings =
    Settings (Just [1]) Nothing processSeason parallelProcessor
