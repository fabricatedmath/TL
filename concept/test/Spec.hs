{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import qualified Crypto.Hash as Hash
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16)
import Data.Serialize

import Crypto.TL
import Crypto.TL.Primitives (Hash(..), hashOnce)
import Crypto.TL.Types

import Impls
import TestVectors

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Crytpto.TL.Primitives" $ do
        it "Check SHA256 Sanity From Library" $ do
            hashAbc `shouldBe` hashAbcGroundTruth

    specImpls

specImpls :: Spec
specImpls = do
  testHash "X86" shaModeX86
  testHash "Generic" shaModeGeneric
  testHash "Arm" shaModeArm
  testHash "Native" shaModeNative

testHash :: HasHashFunc a => String -> HashMode a -> Spec
testHash name mode = do
    describe ("Crypto.TL.Impls." <> name) $ do
      ehashFunc <- runIO $ getHashFunc mode
      case ehashFunc of 
        Left message -> it ("Skipping due to: " <> message) $ () `shouldBe` ()
        Right hashFunc -> do
          it "Hash Sanity Check" $ do
            hashFunc 1 hashAbc `shouldBe` hashAbcGroundTruthIter1
          it "Hash Sanity Check 2" $ do
            hashFunc 1 hashAbcGroundTruthIter1 `shouldBe` hashAbcGroundTruthIter2
          it "Hash Sanity Check 3" $ do
            hashFunc 2 hashAbc `shouldBe` hashAbcGroundTruthIter2
          describe "Hash Chain" $ do
            specChain hashFunc $ createChain hashFunc
          describe "Hash Chain (Parallel)" $ do
            specChain hashFunc $ createChainParallel hashFunc


specChain :: HashFunc -> (Int -> Int -> IO (Maybe (Hash,ChainHead))) -> Spec
specChain hashFunc f = 
    do
        it "Chain Serialize/Deserialize" $ do
            Just (_hash, chain) <- f 10 10
            Right chain `shouldBe` decode (encode chain)

        it "Create Chain and Solve Chain" $ do
            Just (hash, chain) <- f 10 10
            Right hash `shouldBe` solveChain hashFunc chain

