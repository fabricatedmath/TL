module TL.Solve where

import Control.Monad.Except (liftIO, runExceptT)

import qualified Data.ByteString as BS (readFile)
import Data.Serialize

import Options.Applicative

import Crypto.TL

import TL.Util

data Solve = Solve FilePath FilePath
  deriving Show

solveParser :: Parser Solve
solveParser = Solve 
  <$> strOption
  ( long "file"
  <> short 'f'
  <> help "TimeLock file to solve"
  <> metavar "FILENAME"
  )
  <*> strOption
  ( long "out"
  <> short 'o'
  <> help "Name of file to output"
  <> metavar "FILENAME"
  )

solve :: Mode -> Solve -> IO ()
solve mode (Solve inFile outFile) = do
  echain <- runGet get <$> BS.readFile inFile
  case echain of
    Left err -> putStrLn err
    Right chain -> do
      case getSolvingFunc mode chain of
        Left err -> putStrLn err
        Right hash -> do
          eres <- runExceptT $ decryptTLA inFile outFile hash
          case eres of 
            Left err -> putStrLn err
            Right () -> putStrLn $ "Wrote file to " <> outFile

getSolvingFunc :: Mode -> (ChainHead -> Either String Hash)
getSolvingFunc Slow = solveChain slowMode
getSolvingFunc Fast = solveChain fastMode