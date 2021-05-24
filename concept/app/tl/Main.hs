module Main where

import Data.List
import Data.Monoid
import Options.Applicative

import Crypto.TL (HashFunc)

import TL.Create
import TL.Solve
import TL.Bench
import TL.Util

data Purpose = PurposeCreate Create | PurposeSolve Solve | PurposeBench Bench
  deriving Show

data TL = TL Purpose

tl :: Parser TL
tl = TL <$> purpose

purpose :: Parser Purpose
purpose = subparser
       ( command "create"
         (info (PurposeCreate <$> createParser <**> helper)
               (progDesc "Create a TimeLock file"))
      <> command "solve"
         (info (PurposeSolve <$> solveParser <**> helper)
               (progDesc "Solve a TimeLock file"))
      <> command "bench"
         (info (PurposeBench <$> benchParser <**> helper)
               (progDesc "Benchmark Hashing Functions"))
       )

run :: TL -> IO ()
run (TL purpose) = 
  case purpose of
    (PurposeCreate c) -> create c
    (PurposeSolve s) -> solve s
    (PurposeBench b) -> bench b

opts :: ParserInfo TL
opts = 
  info (tl <**> helper) 
  ( fullDesc
  <> progDesc "TimeLock"
  <> header "hello"
  )

main :: IO ()
main = customExecParser (prefs showHelpOnEmpty) opts >>= run