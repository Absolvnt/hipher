module Cipher.PolyalphabeticSpec where

import           Cipher              (fromString, toString)
import           Cipher.Polyalphabetic
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
-- TODO: Add some more rigorous testing for the ciphers
  describe "vigenere" $ do
    it "applies a Viginere cipher" $ do
      toString (vigenere (fromString "hask") $ fromString "helloworld") `shouldBe` "oedvvwgbsd"
      
  describe "unvigenere" $ do
    it "is the inverse (decipher) of vigenere" $ do
      toString (unvigenere (fromString "hask") $ fromString "oedvvwgbsd") `shouldBe` "helloworld"  