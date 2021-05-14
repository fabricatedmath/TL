module Main where

import Data.List
import Data.Monoid
import Options.Applicative

import Crypto.TL (HashFunc)

import TL.Create
import TL.Solve
import TL.Util

data Purpose = PurposeCreate Create | PurposeSolve Solve
  deriving Show

data TL = TL HashFunc Purpose

tl :: Parser HashFunc -> Parser TL
tl hashFunc = TL <$> hashFunc <*> purpose

purpose :: Parser Purpose
purpose = subparser
       ( command "create"
         (info (PurposeCreate <$> createParser <**> helper)
               (progDesc "Create a TimeLock file"))
      <> command "solve"
         (info (PurposeSolve <$> solveParser <**> helper)
               (progDesc "Solve a TimeLock file"))
       )

run :: TL -> IO ()
run (TL haskFunc purpose) = 
  case purpose of
    (PurposeCreate c) -> create haskFunc c
    (PurposeSolve s) -> solve haskFunc s

opts :: Parser HashFunc -> ParserInfo TL
opts hashFunc = 
  info (tl hashFunc <**> helper) 
  ( fullDesc
  <> progDesc "TimeLock"
  <> header "hello"
  )

main :: IO ()
main = 
  do
    hashFunc <- hashFuncParser
    customExecParser (prefs showHelpOnEmpty) (opts hashFunc) >>= run