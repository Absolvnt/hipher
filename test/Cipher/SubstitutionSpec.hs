module Cipher.SubstitutionSpec where

import           Cipher              (cipher)
import           Cipher.Substitution
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do

  describe "caesar" $ do
    it "applies a Caeser cipher" $ do
      cipher (caesar 25) "HELLOWORLD" `shouldBe` "GDKKNVNQKC"
      
  describe "uncaesar" $ do
    it "is the inverse (decipher) of caesar" $ do
      cipher (uncaesar 25) "GDKKNVNQKC" `shouldBe` "HELLOWORLD"
