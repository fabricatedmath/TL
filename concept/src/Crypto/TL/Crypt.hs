module Crypto.TL.Crypt 
    ( decrypt, encrypt
    ) where

import Data.Word (Word64)

import Foreign.C.String (CString, withCString)

import Crypto.TL.Primitives (Hash, unsafeUseAsCString)

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

foreign import ccall safe "decrypt_offset"
    c_decrypt_offset :: CString -> CString -> Word64 -> CString -> IO Int

foreign import ccall safe "encrypt_offset"
    c_encrypt_offset :: CString -> CString -> Word64 -> CString -> IO Int