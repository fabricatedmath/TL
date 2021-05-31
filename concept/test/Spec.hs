{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Control.Monad (replicateM)
import Control.Monad.Except (runExceptT)
import Data.Serialize

import Crypto.TL

import Crypto.TL.Archive.Header
import Crypto.TL.Primitives.Checksum
import Crypto.TL.Primitives.ShortHash
import Crypto.TL.Primitives.Tower

import TestVectors

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Crytpto.TL.Primitives" $ do
      it "Check SHA256 Sanity From Library" $ do
        hashAbc `shouldBe` hashAbcGroundTruth
    describe "Crypto.TL.Types" $ do
      it "Check Show Hash" $ do
        show hashAbc `shouldBe` "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
      it "Check Hash" $ do
        stringToHash "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" `shouldBe` Just hashAbc
    describe "Crypto.TL.Primitives" $ do
      it "Test Magic Hash 1" $ do
        show magicHash `shouldBe` "ad830d9906e6264093497b5e51b56ff56c7980884a37aeb3289352fa702da3b9"
      it "Test Magic Hash 2" $ do
        hashToShortHash magicHash `shouldBe` shortHashFlipEndian (ShortHash 0xad830d99 0x06e62640)
      it "Test Magic Hash 2" $ do
        show (hashToShortHash magicHash) `shouldBe` "ad830d9906e62640"

    describe "Crypto.TL.Tyes" $ do
      it "Test Hash Increment 1" $ do
        let hash = Hash 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0xffffffff
        let hash' = Hash 0x0 0x0 0x0 0x0 0x0 0x0 0x1 0x0
        incrementHash hash `shouldBe` hash'
      it "Test Hash Increment 2" $ do
        let hash = Hash 0xffffffff 0xffffffff 0xffffffff 0xffffffff 0xffffffff 0xffffffff 0xffffffff 0xffffffff
        let hash' = Hash 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0
        incrementHash hash `shouldBe` hash'
      it "Test Hash Increment 3" $ do
        let hash = Hash 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0
        let hash' = Hash 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x00000001
        incrementHash hash `shouldBe` hash'
      it "Test Hash Increment 4" $ do
        let hash = Hash 0xfffffffe 0xfffffffe 0xfffffffe 0xfffffffe 0xfffffffe 0xfffffffe 0xfffffffe 0xffffffff
        let hash' = Hash 0xfffffffe 0xfffffffe 0xfffffffe 0xfffffffe 0xfffffffe 0xfffffffe 0xffffffff 0x0
        incrementHash hash `shouldBe` hash'

    specPacked
    specHashImpls
    specBulkHashImpls
    specChain
    specHeader

specHashImpls :: Spec
specHashImpls = do
  specHash "X86" shaModeX86
  specHash "Generic" shaModeGeneric
  specHash "Arm" shaModeArm
  specHash "Native" shaModeNative

specHash :: HasHashFunc a => String -> HashMode a -> Spec
specHash name mode = do
    describe ("Crypto.TL.Impls." <> name) $ do
      ehashFunc <- runIO $ getHashFunc mode
      case ehashFunc of 
        Left message -> it ("Skipping due to: " <> message) $ () `shouldBe` ()
        Right hashFunc -> do
          it "Hash Sanity Check" $ do
            hashAbcIter1 <- hashFunc 1 hashAbc
            hashAbcIter1 `shouldBe` hashAbcGroundTruthIter1
          it "Hash Sanity Check 2" $ do
            hashAbcIter2 <- hashFunc 1 hashAbcGroundTruthIter1
            hashAbcIter2 `shouldBe` hashAbcGroundTruthIter2
          it "Hash Sanity Check 3" $ do
            hashAbcIter2 <- hashFunc 2 hashAbc
            hashAbcIter2 `shouldBe` hashAbcGroundTruthIter2

specBulkHashImpls :: Spec
specBulkHashImpls = do
  specBulkHash "X86" shaModeBulkX86
  specBulkHash "Generic" shaModeBulkX86
  specBulkHash "Arm" shaModeBulkX86
  specBulkHash "Native" shaModeBulkX86
  specBulkHash "Cuda" shaModeCuda

--todo: get programmatic info about cuda topology
--todo: Manage error codes
specBulkHash :: HasBulkHashFunc a => String -> HashMode a -> Spec
specBulkHash name mode = do
  describe ("Crypto.TL.Impls." <> name) $ do
    ebulkHashFunc <- runIO $ getBulkHashFunc mode
    case ebulkHashFunc of
      Left message -> it ("Skipping due to: " <> message) $ () `shouldBe` ()
      Right (_capabilities, bulkHashFunc) -> do
        it "Bulk Hash Sanity Check" $ do
          hashes' <- bulkHashFunc 1 (take 10 $ repeat hashAbc)
          hashes' `shouldBe` map (Tower hashAbc) (take 10 $ repeat hashAbcGroundTruthIter1)
        it "Bulk Hash Sanity Check 2" $ do
          hashes' <- bulkHashFunc 2 (take 10 $ repeat hashAbc)
          hashes' `shouldBe` map (Tower hashAbc) (take 10 $ repeat hashAbcGroundTruthIter2)
        it "Bulk Hash Sanity Check 3" $ do
          hashes' <- bulkHashFunc 1 (take 10 $ repeat hashAbcGroundTruthIter1)
          hashes' `shouldBe` map (Tower hashAbcGroundTruthIter1) (take 10 $ repeat hashAbcGroundTruthIter2)

specPacked :: Spec
specPacked = do
  describe "Crypto.TL.Bulk" $ do
    it "Packed Test" $ do
      hashes <- replicateM 10 randomHash
      hashes `shouldBe` toUnpacked (toPacked hashes)

specHeader :: Spec
specHeader = do
  describe "Crypto.TL.Archive.Header" $ do
    it "Header Test" $ do
      Right hashFunc <- getHashFunc shaModeGeneric
      Right (_capabilities, bulkHashFunc) <- getBulkHashFunc shaModeBulkGeneric
      let tlaFileName = TLAFileName $ TLAText $ "Dogs.txt"
      Just (_hash, chainHead) <- createChain hashFunc bulkHashFunc 10 10
      let header = TLAHeader MagicHash tlaCurrentVersion tlaShaHashMethod chainHead tlaFileName
      Right header `shouldBe` decode (encode header)

specChain :: Spec
specChain = 
    do
      describe "Crypto.TL.Primitives.Chain" $ do
        it "Chain Serialize/Deserialize" $ do
          Right hashFunc <- getHashFunc shaModeGeneric
          Right (_capabilities, bulkHashFunc) <- getBulkHashFunc shaModeBulkGeneric
          Just (_hash, chain) <- createChain hashFunc bulkHashFunc 10 10
          Right chain `shouldBe` decode (encode chain)

        it "Create Chain and Solve Chain" $ do
          Right hashFunc <- getHashFunc shaModeGeneric
          Right (_capabilities, bulkHashFunc) <- getBulkHashFunc shaModeBulkGeneric
          Just (hash, chain) <- createChain hashFunc bulkHashFunc 10 10
          ehash <- runExceptT $ solveChain hashFunc chain
          Right hash `shouldBe` ehash
