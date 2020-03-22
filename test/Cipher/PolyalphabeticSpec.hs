module Cipher.PolyalphabeticSpec where

import           Cipher              (fromString, cipher)
import           Cipher.Polyalphabetic
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do

  describe "vigenere" $ do
    it "applies a Viginere cipher" $ do
      cipher (vigenere $ fromString "HASK") "HELLOWORLD" `shouldBe` "OEDVVWGBSD"
      
  describe "unvigenere" $ do
    it "is the inverse (decipher) of vigenere" $ do
      cipher (unvigenere $ fromString "HASK") "OEDVVWGBSD" `shouldBe` "HELLOWORLD"  