{-# LANGUAGE FlexibleContexts #-}

module Crypto.TL.Crypt 
    ( decryptTLA, encryptTLA
    ) where

import Control.Monad (when)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.ByteString as BS (length, readFile, writeFile)
import Data.Serialize (get, encode, runGetState)

import Data.Word (Word64)

import Foreign.C.String (CString, withCString)

import Crypto.TL.Chain (ChainHead)
import Crypto.TL.Primitives (Hash, unsafeUseAsCString)

encryptTLA
    :: (MonadIO m, MonadError String m)
    => FilePath -- targetFile
    -> FilePath -- sourceFile
    -> (Hash, ChainHead) 
    -> m ()
encryptTLA targetFile sourceFile (hash, chain) = 
    do
        let bs = encode chain
        liftIO $ BS.writeFile targetFile bs
        errCode <- liftIO $ encrypt targetFile sourceFile (BS.length bs) hash
        when (errCode /= 0) $ throwError "Failed to encrypt file"

decryptTLA
    :: (MonadIO m, MonadError String m)
    => FilePath -- targetFile
    -> FilePath -- sourceFile
    -> Hash
    -> m ()
decryptTLA targetFile sourceFile hash = 
    do
        bs <- liftIO $ BS.readFile sourceFile 
        let edecode = runGetState get bs 0
        case edecode of 
            Left err -> throwError $ "Failed to decode chain: " ++ err
            Right (chain,_bs) -> 
                do
                    errCode <- liftIO $ decrypt targetFile sourceFile (chainByteLen chain) hash
                    when (errCode /= 0) $ throwError "Failed to decrypt file"

chainByteLen :: ChainHead -> Int
chainByteLen = BS.length . encode

-- Can clean up with Continuation Monad, gross
encrypt
    :: FilePath
    -> FilePath
    -> Int 
    -> Hash
    -> IO Int
encrypt targetFile sourceFile offset hash = 
    do
        let offsetW = fromIntegral offset
        withCString targetFile (\tf -> 
            withCString sourceFile  (\sf -> 
                    unsafeUseAsCString hash $ 
                        c_encrypt_offset tf sf offsetW
                )
            )

decrypt
    :: FilePath
    -> FilePath
    -> Int 
    -> Hash
    -> IO Int
decrypt targetFile sourceFile offset hash = 
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
