module Main where

import Crypto.TL

import qualified Data.ByteString as BS (readFile)

import Data.Serialize (decode)

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
main =
    do
        args <- getArgs
        let (actions, nonOptions, errors) = getOpt RequireOrder options args
        opts <- foldl (>>=) (return startOptions) actions
        let Options 
                { optMode = mode
                , optFilePath  = mfp
                } = opts
        case mfp of
            Nothing ->
                putStrLn "No filepath supplied, exiting..."
            Just fp -> 
                do
                    echain <- decode <$> BS.readFile fp
                    case echain of
                        Left msg -> error msg
                        Right chain -> 
                            do
                                let ehash = 
                                        either
                                        (\l -> solveChain l chain)
                                        (\r -> solveChain r chain)
                                        mode
                                case ehash of
                                    Left msg -> error msg
                                    Right hash -> 
                                        do
                                            putStrLn ""
                                            putStrLn $ "Super Secret Hash: " ++ show hash
                                            putStrLn ""
