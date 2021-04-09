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

specChain :: (Int -> Int -> IO (Maybe (Hash,ChainHead))) -> Spec
specChain f = 
    do
        it "Chain Serialize/Deserialize" $ do
            Just (_hash, chain) <- f 10 10
            Right chain `shouldBe` decode (encode chain)

        it "Create Chain and Solve Chain" $ do
            Just (hash, chain) <- f 10 10
            Right hash `shouldBe` solveChain fastMode chain

spec :: Spec
spec = do
    describe "Crytpto.TL.Primitives" $ do
        it "Check SHA256 Sanity From Library" $ do
            let 
                ehash = decodeBase16 "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
                hash' :: ByteString
                hash' = ByteArray.convert $ Hash.hashWith Hash.SHA256 ("abc" :: ByteString)
            case ehash of
                Left err -> error $ show err
                Right hash -> hash' `shouldBe` hash

        it "Check Haskell vs C iterations are equal (1000)" $ do
            let hash = hashDefault "abc"
                numIter = 1000
            hashIter slowMode numIter hash `shouldBe` hashIter fastMode numIter hash

        it "Hash Serialize/Deserialize" $ do
            let hash = hashDefault "abc"
            Right hash `shouldBe` decode (encode hash)

    describe "Crypto.TL.Chain (Fast)" $ do
        specChain (createChain fastMode)

    describe "Crypto.TL.Chain (Slow)" $ do
        specChain (createChain slowMode)

    describe "Crypto.TL.Chain.Parallel (Fast)" $ do
        specChain (createChainParallel fastMode)

    describe "Crypto.TL.Chain.Parallel (Slow)" $ do
        specChain (createChainParallel slowMode)
