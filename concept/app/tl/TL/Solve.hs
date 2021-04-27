module TL.Solve where

import Options.Applicative

import TL.Util

data Solve = Solve FilePath
  deriving Show

solveParser :: Parser Solve
solveParser = Solve 
  <$> strOption
  ( long "file"
  <> short 'f'
  <> help "TimeLock file to solve"
  <> metavar "FILENAME"
  )

solve :: Mode -> Solve -> IO ()
solve mode (Solve fp) = return ()