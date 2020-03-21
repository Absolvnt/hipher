module Cipher.SubstitutionSpec where

import           Cipher              (fromString, toString)
import           Cipher.Substitution
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
-- TODO: Add some more rigorous testing for the ciphers
  describe "caesar" $ do
    it "applies a Caeser cipher" $ do
      toString (caesar 25 $ fromString "helloworld") `shouldBe` "gdkknvnqkc"
      
  describe "uncaesar" $ do
    it "is the inverse (decipher) of caesar" $ do
      toString (uncaesar 25 $ fromString "gdkknvnqkc") `shouldBe` "helloworld"
