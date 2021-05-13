{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import qualified Crypto.Hash as Hash
import Crypto.TL
import Crypto.TL.Primitives (Hash(..), hashOnce)

import qualified Data.ByteArray as ByteArray

import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16)
import Data.Serialize

import TestVectors
import Impls

checkHashingFunc :: (Hashable a) => HashMode a -> Spec
checkHashingFunc mode = 
    do
        it "Check SHA256 Iteration Sanity 1" $ do
            hashOnce mode hashAbcGroundTruth `shouldBe` hashAbcGroundTruthIter1
        it "Check SHA256 Iteration Sanity 2" $ do
            hashOnce mode hashAbcGroundTruthIter1 `shouldBe` hashAbcGroundTruthIter2
        it "Check SHA256 Iteration Sanity 3" $ do
            hashIter mode 2 hashAbcGroundTruth `shouldBe` hashAbcGroundTruthIter2

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
            Right hash `shouldBe` solveChain cryptoniteMode chain

spec :: Spec
spec = do
    describe "Crytpto.TL.Primitives" $ do
        it "Check SHA256 Sanity From Library" $ do
            hashAbc `shouldBe` hashAbcGroundTruth
{-        
        it "Check Haskell vs C iterations are equal (1000)" $ do
            let hash = hashDefault "abc"
                numIter = 1000
            hashIter slowMode numIter hash `shouldBe` hashIter fastMode numIter hash

        it "Hash Serialize/Deserialize" $ do
            let hash = hashDefault "abc"
            Right hash `shouldBe` decode (encode hash)
            -}

    describe "Crypto.TL.Hashing.Generic" $ do
        checkHashingFunc shaGenericMode 
    
    describe "Crypto.TL.Hashing.Cryptonite" $ do
        checkHashingFunc cryptoniteMode

    describe "Crypto.TL.Hashing.X86" $ do
        checkHashingFunc shax86Mode

    describe "Crypto.TL.Chain (Cryptonite)" $ do
        specChain (createChain cryptoniteMode)

    describe "Crypto.TL.Chain (Generic)" $ do
        specChain (createChain shaGenericMode)

    describe "Crypto.TL.Chain (X86)" $ do
        specChain (createChain shax86Mode)

    describe "Crypto.TL.Chain.Parallel (Cryptonite)" $ do
        specChain (createChainParallel cryptoniteMode)

    describe "Crypto.TL.Chain.Parallel (Generic)" $ do
        specChain (createChainParallel shaGenericMode)

    describe "Crypto.TL.Chain.Parallel (X86)" $ do
        specChain (createChainParallel shax86Mode)

    specImpls