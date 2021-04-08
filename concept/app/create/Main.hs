module Main where

import Crypto.TL

import qualified Data.ByteString as BS (writeFile)

import Data.Serialize (encode)

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.IO 

data Options = 
    Options 
    { optNumTowers :: Int
    , optTowerHeight :: Int
    , optMode :: Either SlowMode FastMode
    , optFilePath :: Maybe FilePath
    } deriving Show

defaultNumTowers :: Int
defaultNumTowers = 10

defaultTowerHeight :: Int
defaultTowerHeight = 10

startOptions :: Options
startOptions = 
    Options
    { optNumTowers = defaultNumTowers
    , optTowerHeight = defaultTowerHeight
    , optMode = Right fastMode
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
        ( NoArg (\opt -> return opt { optMode = Left slowMode })
        ) "Switch to slow mode (non sse/sha optimized x86)"
    , Option "f" ["filepath"]
        ( ReqArg (\arg opt -> return opt { optFilePath = Just arg }) "FILEPATH"
        ) "Output File"
    , Option "h" ["help"]
        ( NoArg (\_ -> do
            prg <- getProgName
            hPutStrLn stderr (usageInfo prg options)
            exitWith ExitSuccess
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
                , optFilePath  = mfp
                } = opts
        (hash, chain) <- 
            either 
            (\l -> createChain l numTowers towerHeight) 
            (\r -> createChain r numTowers towerHeight)
            mode

        putStrLn ""
        putStrLn $ "Super Secret Hash: " ++ show hash
        putStrLn ""
        print chain

        case mfp of
            Nothing -> putStrLn "No FilePath supplied"
            Just fp -> BS.writeFile fp $ encode chain
                    