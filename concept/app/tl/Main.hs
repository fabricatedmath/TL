module Main where

import Data.List
import Data.Monoid
import Options.Applicative

data Mode = Slow | Fast
  deriving Show

data Concurrency = Serial | Parallel
    deriving Show

data TL
  = Create Int Int FilePath FilePath
  | Solve FilePath
  deriving (Eq, Show)

create :: Parser TL
create = Create 
  <$> option auto
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

solve :: Parser TL
solve = Solve 
  <$> strOption
  ( long "file"
  <> short 'i'
  <> help "TimeLock file to solve"
  <> metavar "FILENAME"
  )

tl :: Parser TL
tl = subparser
       ( command "create"
         (info (create <**> helper)
               (progDesc "Create a TimeLock file"))
      <> command "solve"
         (info (solve <**> helper)
               (progDesc "Solve a TimeLock file"))
       )

run :: TL -> IO ()
run (Create numTowers numIters inFile outFile) = putStrLn $ "Creating TimeLock file with " ++ show numTowers ++ " towers with " ++ show numIters ++ " iterations per tower"
run (Solve inFile) = putStrLn $ "Solving TimeLock file \'" <> inFile <> "\'"

opts :: ParserInfo TL
opts = 
  --info (tl <**> helper) idm
  info (tl <**> helper) 
  ( fullDesc
  <> progDesc "TimeLock"
  <> header "hello"
  )

main :: IO ()
main = customExecParser (prefs showHelpOnEmpty) opts >>= run