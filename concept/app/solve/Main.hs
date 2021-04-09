module Main where

import Crypto.TL

import System.Console.GetOpt

import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import System.IO 

data Options = 
    Options 
    { optMode :: Either SlowMode FastMode
    , optFilePath :: Maybe FilePath 
    } deriving Show

startOptions :: Options
startOptions = 
    Options
    { optMode = Right fastMode
    , optFilePath = Nothing
    }

options :: [OptDescr (Options -> IO Options)]
options = 
    [ Option "s" ["slowmode"]
        ( NoArg (\opt -> return opt { optMode = Left slowMode })
        ) "Switch to slow mode (non sse/sha optimized x86)"
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
main = print "Solve"