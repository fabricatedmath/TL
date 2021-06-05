{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Crypto.TL.Util.HashesFromFile 
  ( getFilesChecksumMap
  ) where

import Control.Monad.Except (ExceptT(..), MonadError(..), withExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr

import Crypto.TL.Primitives.Checksum
import Crypto.TL.Primitives.Hash
import Crypto.TL.Primitives.HashFunc

hashFinderErrorToString :: HashFinderError -> String
hashFinderErrorToString (HashFinderFailedToOpenFile fp) = "Failed to open file: " <> show fp
hashFinderErrorToString (HashFinderFailedToCloseFile fp) = "Failed to properly close file: " <> show fp

getFilesChecksumMap :: MonadIO m => HashFunc -> FilePath -> ExceptT String m (Map Checksum Hash)
getFilesChecksumMap hashFunc fp = withExceptT hashFinderErrorToString $ do
  !set <- getFilesHashes 4096 fp
  checksumList <- liftIO $ mapM (\hash -> (,hash) <$> calcChecksum hashFunc hash) $ Set.toList set
  let !checksumMap = Map.fromList checksumList
  return checksumMap

data HashFinderError = HashFinderFailedToOpenFile FilePath | HashFinderFailedToCloseFile FilePath
  deriving Show

initHashFinder :: (MonadError HashFinderError m, MonadIO m) => FilePath -> m (ForeignPtr HashFinderHandle)
initHashFinder fp = do
  fptrHashFinder <- liftIO $ c_newHashFinder >>= newForeignPtr c_deleteHashFinder
  initResult <- liftIO $ withForeignPtr fptrHashFinder (\ptr -> withCString fp (c_initializeHashFinder ptr))
  case initResult of
    0 -> return fptrHashFinder
    _ -> throwError $ HashFinderFailedToOpenFile fp

deinitHashFinder :: (MonadError HashFinderError m, MonadIO m) => FilePath -> ForeignPtr HashFinderHandle -> m ()
deinitHashFinder fp fptrHashFinder = do
  deinitResult <- liftIO $ withForeignPtr fptrHashFinder c_deinitializeHashFinder
  case deinitResult of
    0 -> liftIO $ finalizeForeignPtr fptrHashFinder
    _ -> throwError $ HashFinderFailedToCloseFile fp

getFilesHashes :: (MonadError HashFinderError m, MonadIO m) => Int -> FilePath -> m (Set Hash)
getFilesHashes buffSize' fp = do
  fptrHashFinder <- initHashFinder fp
  let buffSize = fromIntegral buffSize'

  hashStringsPtr <- liftIO $ mallocArray buffSize'
  let 
    go !prevSet = do
      numHashesWritten <- fromIntegral <$> withForeignPtr fptrHashFinder (\ptr -> c_readHexesHashFinder ptr buffSize hashStringsPtr)
      case numHashesWritten of
        0 -> return prevSet
        _ -> do
          !thisSet <- Set.fromList <$> peekArray numHashesWritten hashStringsPtr
          let !nextSet = Set.union prevSet thisSet
          go nextSet
  !set <- liftIO $ go Set.empty
  liftIO $ free hashStringsPtr
  deinitHashFinder fp fptrHashFinder 
  return set

data HashFinderHandle

foreign import ccall safe "newHashFinder" c_newHashFinder
    :: IO (Ptr HashFinderHandle)

foreign import ccall safe "initializeHashFinder" c_initializeHashFinder
  :: Ptr HashFinderHandle -> CString -> IO Int32

foreign import ccall safe "readHexesHashFinder" c_readHexesHashFinder
  :: Ptr HashFinderHandle -> Int32 -> Ptr Hash -> IO Int32

foreign import ccall safe "deinitializeHashFinder" c_deinitializeHashFinder
  :: Ptr HashFinderHandle -> IO Int32

foreign import ccall safe "&deleteHashFinder" c_deleteHashFinder
    :: FinalizerPtr HashFinderHandle