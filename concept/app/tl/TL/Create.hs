module TL.Create where

import Options.Applicative

import TL.Util

data Concurrency = Serial | Parallel
    deriving Show

data Create = Create Concurrency Int Int FilePath FilePath
    deriving Show

createParser :: Parser Create
createParser = Create 
  <$> fromBool Serial Parallel <$> switch
    ( long "serial"
    <> help "Run tower creation in serial (rather than in parallel)"
    )
  <*> option auto
    ( long "numtowers"
    <> short 'n'
    <> help "Number of towers to build"
    <> metavar "INT"
    )
  <*> option auto
    ( long "iters"
    <> short 'i'
    <> help "Number of hash iterations per tower"
    <> metavar "INT"
    )
  <*> strOption
    ( long "file"
    <> short 'f'
    <> help "File to encrypt with TimeLock"
    <> metavar "FILENAME"
    )
  <*> strOption
    ( long "out"
    <> short 'o'
    <> help "Name of TimeLock file to output"
    <> metavar "FILENAME"
    )

create :: Mode -> Create -> IO ()
create mode (Create concurrency numTowers numIters inFile outFile) = return ()