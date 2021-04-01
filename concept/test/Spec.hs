{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Crypto.TL

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
        it "Hash check" $ do
            let 
                bs = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
                ehash = Hash . BS256 <$> decodeBase16 bs
            case ehash of
                Left err -> error $ show err
                Right hash -> sha256 "abc" `shouldBe` hash