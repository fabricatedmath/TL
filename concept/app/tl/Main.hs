module Main where

import Data.List
import Data.Monoid
import Options.Applicative

import Crypto.TL (Mode)

import TL.Create
import TL.Solve
import TL.Util

data Purpose = PurposeCreate Create | PurposeSolve Solve
  deriving Show

data TL = TL Mode Purpose
  deriving Show

tl :: Parser TL
tl = TL <$> mode <*> purpose

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
run (TL mode purpose) = 
  case purpose of
    (PurposeCreate c) -> create mode c
    (PurposeSolve s) -> solve mode s

opts :: ParserInfo TL
opts = 
  info (tl <**> helper) 
  ( fullDesc
  <> progDesc "TimeLock"
  <> header "hello"
  )

main :: IO ()
main = customExecParser (prefs showHelpOnEmpty) opts >>= run