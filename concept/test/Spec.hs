{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import qualified Crypto.Hash as Hash
import Crypto.TL

import qualified Data.ByteArray as ByteArray

import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16)
import Data.Serialize

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Crypto.TL" $ do
        it "Chain Serialize/Deserialize" $ do
            ehchain <- createChain 10 10
            case ehchain of
                Left err -> error $ show err
                Right (_hash, chain) -> 
                    do
                        let bs = encode chain
                        Right chain `shouldBe` decode bs

        it "Create Chain and Solve Chain" $ do
            ehchain <- createChain 10 10
            case ehchain of
                Left err -> error $ show err
                Right (hash, chain) -> Right hash `shouldBe` solveChain chain
    describe "Crytpto.TL" $ do
        it "Check SHA256 Sanity From Library" $ do
            let 
                ehash = decodeBase16 "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

                hash' :: ByteString
                hash' = ByteArray.convert $ Hash.hashWith Hash.SHA256 ("abc" :: ByteString)
            case ehash of
                Left err -> error $ show err
                Right hash -> hash' `shouldBe` hash