{-# LANGUAGE TemplateHaskell #-}

module Test where

import Control.Exception (tryJust)
import Control.Monad (guard)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (readFile)

import Data.FileEmbed (bsToExp)

import System.Directory (doesFileExist)
import System.IO.Error (isDoesNotExistError)

import Language.Haskell.TH.Syntax (Exp, Q, runIO, lift)

maybeEmbedFile :: FilePath -> Q Exp
maybeEmbedFile fp = runIO (maybeFile fp) >>= maybeBsToExp
  
maybeBsToExp :: Maybe ByteString -> Q Exp
maybeBsToExp Nothing = [| Nothing |]
maybeBsToExp (Just bs) = [| Just $(bsToExp bs) |]

maybeFile :: FilePath -> IO (Maybe ByteString)
maybeFile fp = eitherToMaybe <$> tryJust (guard . isDoesNotExistError) (BS.readFile fp)

eitherToMaybe :: Either () a -> Maybe a
eitherToMaybe (Left ()) = Nothing
eitherToMaybe (Right a) = Just a
