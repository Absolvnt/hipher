module Cipher.SubstitutionSpec where

import           Cipher              (cipher)
import           Cipher.Substitution
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
-- TODO: Add some more rigorous testing for the ciphers
  describe "caesar" $ do
    it "applies a Caeser cipher" $ do
      cipher (caesar 25) "helloworld" `shouldBe` "gdkknvnqkc"
      
  describe "uncaesar" $ do
    it "is the inverse (decipher) of caesar" $ do
      cipher (uncaesar 25) "gdkknvnqkc" `shouldBe` "helloworld"
