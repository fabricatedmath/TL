module Impls where

import Test.Hspec

import Crypto.TL.Impls.Arm
import Crypto.TL.Impls.Generic
import Crypto.TL.Impls.X86

import Crypto.TL.Util

import TestVectors

-- TODO: HSpec message for notification of skipping due to no exts or compilation
specImpls :: Spec
specImpls = do
  testFFIHash "X86" shaModeX86
  testFFIHash "Generic" shaModeGeneric
  testFFIHash "Arm" shaModeArm

testFFIHash :: FFIHashable a => String -> HashMode a -> Spec
testFFIHash name mode = do
    describe ("Crypto.TL.Impls." <> name) $ do
      ehashFunc <- runIO $ ffiHashFunc mode
      case ehashFunc of 
        Left message -> it ("Skipping because of: " <> message) $ () `shouldBe` ()
        Right hashFunc -> do
          it "Hash Sanity Check" $ do
            hashFunc 1 hashAbc `shouldBe` hashAbcGroundTruthIter1
          it "Hash Sanity Check 2" $ do
            hashFunc 2 hashAbc `shouldBe` hashAbcGroundTruthIter2