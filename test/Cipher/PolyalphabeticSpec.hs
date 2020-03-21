module Cipher.PolyalphabeticSpec where

import           Cipher              (fromString, cipher)
import           Cipher.Polyalphabetic
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do

  describe "vigenere" $ do
    it "applies a Viginere cipher" $ do
      cipher (vigenere $ fromString "hask") "helloworld" `shouldBe` "oedvvwgbsd"
      
  describe "unvigenere" $ do
    it "is the inverse (decipher) of vigenere" $ do
      cipher (unvigenere $ fromString "hask") "oedvvwgbsd" `shouldBe` "helloworld"  