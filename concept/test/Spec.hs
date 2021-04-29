{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import qualified Crypto.Hash as Hash
import Crypto.TL
import Crypto.TL.Primitives (Hash(..), hashOnce)

import qualified Data.ByteArray as ByteArray

import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16)
import Data.Serialize

hashAbc :: Hash
hashAbc = Hash hash
    where hash = ByteArray.convert $ Hash.hashWith Hash.SHA256 ("abc" :: ByteString)

hashAbcGroundTruth :: Hash 
hashAbcGroundTruth = Hash hash
    where Right hash = decodeBase16 "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

hashAbcGroundTruthIter1 :: Hash 
hashAbcGroundTruthIter1 = Hash hash
    where Right hash = decodeBase16 "4f8b42c22dd3729b519ba6f68d2da7cc5b2d606d05daed5ad5128cc03e6c6358"

hashAbcGroundTruthIter2 :: Hash 
hashAbcGroundTruthIter2 = Hash hash
    where Right hash = decodeBase16 "f2a778f1a6ed3d5bc59a5d79104c598f3f07093f240ca4e91333fb09ed4f36da"

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
