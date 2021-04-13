{-# LANGUAGE FlexibleContexts #-}

module Crypto.TL.Crypt 
    ( decrypt, encrypt
    ) where

import Control.Monad (when)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.ByteString as BS (length, writeFile, hGet)
import Data.Serialize (encode, runGet)

import Data.Word (Word64)

import Foreign.C.String (CString, withCString)

import System.IO (withFile, IOMode(ReadMode))

import Crypto.TL.Chain (ChainHead, getNumChainBytes)
import Crypto.TL.Primitives (Hash, unsafeUseAsCString)

encrypt
    :: (MonadIO m, MonadError String m)
    => FilePath -- targetFile
    -> FilePath -- sourceFile
    -> (Hash, ChainHead) 
    -> m ()
encrypt targetFile sourceFile (hash, chain) = 
    do
        let bs = encode chain
        liftIO $ BS.writeFile targetFile bs
        errCode <- liftIO $ encryptWithOffset targetFile sourceFile (BS.length bs) hash
        when (errCode /= 0) $ throwError "Failed to encrypt file"

decrypt
    :: (MonadIO m, MonadError String m)
    => FilePath -- targetFile
    -> FilePath -- sourceFile
    -> Hash
    -> m ()
decrypt targetFile sourceFile hash = 
    do
        numBytes <- getChainNumBytes sourceFile
        errCode <- liftIO $ decryptWithOffset targetFile sourceFile numBytes hash
        when (errCode /= 0) $ throwError "Failed to decrypt file"
    where
        getChainNumBytes fp = 
            do
                bs <- liftIO $ withFile fp ReadMode (\h -> BS.hGet h 8)
                liftError $ runGet getNumChainBytes bs

        liftError :: MonadError e m => Either e a -> m a
        liftError (Left e) = throwError e
        liftError (Right a) = pure a

-- Can clean up with Continuation Monad, gross
encryptWithOffset
    :: FilePath
    -> FilePath
    -> Int 
    -> Hash
    -> IO Int
encryptWithOffset targetFile sourceFile offset hash = 
    do
        let offsetW = fromIntegral offset
        withCString targetFile (\tf -> 
            withCString sourceFile  (\sf -> 
                    unsafeUseAsCString hash $ 
                        c_encrypt_offset tf sf offsetW
                )
            )

decryptWithOffset
    :: FilePath
    -> FilePath
    -> Int 
    -> Hash
    -> IO Int
decryptWithOffset targetFile sourceFile offset hash = 
    do
        let offsetW = fromIntegral offset
        withCString targetFile (\tf -> 
            withCString sourceFile  (\sf -> 
                    unsafeUseAsCString hash $ 
                        c_decrypt_offset tf sf offsetW
                )
            )

foreign import ccall safe "encrypt_offset"
    c_encrypt_offset :: CString -> CString -> Word64 -> CString -> IO Int

foreign import ccall safe "decrypt_offset"
    c_decrypt_offset :: CString -> CString -> Word64 -> CString -> IO Int
