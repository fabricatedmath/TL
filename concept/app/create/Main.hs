module Main where

import Crypto.TL

import qualified Data.ByteString as BS (writeFile)

import Data.Serialize (encode)

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import System.IO 

data Mode = Slow | Fast
    deriving Show

data Concurrency = Serial | Parallel
    deriving Show

data Options = 
    Options 
    { optNumTowers :: Int
    , optTowerHeight :: Int
    , optMode :: Mode
    , optConcurrency :: Concurrency
    , optFilePath :: Maybe FilePath
    } deriving Show



defaultNumTowers :: Int
defaultNumTowers = 10

defaultTowerHeight :: Int
defaultTowerHeight = 10

getChainingFunc 
    :: Mode 
    -> Concurrency
    -> (Int -> Int -> IO (Maybe (Hash, ChainHead)))
getChainingFunc Slow Serial = createChain slowMode
getChainingFunc Slow Parallel = createChainParallel slowMode
getChainingFunc Fast Serial = createChain fastMode
getChainingFunc Fast Parallel = createChainParallel fastMode

startOptions :: Options
startOptions = 
    Options
    { optNumTowers = defaultNumTowers
    , optTowerHeight = defaultTowerHeight
    , optMode = Fast
    , optConcurrency = Parallel
    , optFilePath = Nothing
    }

options :: [OptDescr (Options -> IO Options)]
options = 
    [ Option "n" ["num"]
        ( ReqArg (\arg opt -> return opt { optNumTowers = read arg}) "INT"
        ) $ "Number of Towers (Default: " ++ show defaultNumTowers ++ ")"
    , Option "i" ["height"]
        ( ReqArg (\arg opt -> return opt { optTowerHeight = read arg}) "INT"
        ) $ "Towers height (number of iterations per tower) (Default: " ++ show defaultTowerHeight ++ ")"
    , Option "s" ["slowmode"]
        ( NoArg (\opt -> return opt { optMode = Slow })
        ) "Switch to slow mode (non sse/sha optimized x86)"
    , Option "" ["serial"]
        ( NoArg (\opt -> return opt { optConcurrency = Serial })
        ) "Switch to serial mode (Default: Parallel)"
    , Option "f" ["filepath"]
        ( ReqArg (\arg opt -> return opt { optFilePath = Just arg }) "FILEPATH"
        ) "Output File"
    , Option "h" ["help"]
        ( NoArg (\_ -> do
            prg <- getProgName
            hPutStrLn stderr (usageInfo prg options)
            exitSuccess
            )
        ) "Show help"
    ]

main :: IO ()
main = 
    do
        args <- getArgs
        let (actions, nonOptions, errors) = getOpt RequireOrder options args
        opts <- foldl (>>=) (return startOptions) actions
        let Options 
                { optNumTowers = numTowers
                , optTowerHeight = towerHeight
                , optMode = mode
                , optConcurrency = concurrency
                , optFilePath  = mfp
                } = opts
        Just (hash, chain) <- getChainingFunc mode concurrency numTowers towerHeight

        putStrLn ""
        putStrLn $ "Super Secret Hash: " ++ show hash
        putStrLn ""
        print chain

        case mfp of
            Nothing -> putStrLn "No FilePath supplied"
            Just fp -> BS.writeFile fp $ encode chain
                    