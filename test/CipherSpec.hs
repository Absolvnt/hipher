module CipherSpec where

import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Letter" $ do 
      it "produces" $ do 
          True `shouldBe` True
