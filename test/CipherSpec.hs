module CipherSpec where

import           Cipher
import           Data.Char       (toUpper)
import           Data.List       ((\\))
import           Data.Maybe
import           Test.Hspec
import           Test.QuickCheck

letters = ['A' .. 'Z'] ++ ['a' .. 'z'] :: [Char]

nonletters = [minBound .. maxBound] \\ letters :: [Char]

spec :: Spec
spec = do

  describe "Letter" $ do
    it "is equal if its inhabitants are equal modulo 26" $ do
        property $ \x m -> letter x == letter (x + 26*m) 
    it "is ordered according to its inhabitants modulo 26" $ do
        zipWith compare [letter 1, letter 1, letter 1] [letter 25, letter 26, letter 27] `shouldBe` [LT, GT, EQ]
    context "when printed" $ do
      it "displays the upper-case alphabet for 0-25" $ do 
          forAll (elements [0..25]) $ \x -> show (letter x) == show (['A'..'Z'] !! x)
      it "displays the same character given a value modulo 26" $ do
        property $ \x m -> show (letter x) == show (letter $ x + 26*m)
    
  describe "fromChar" $ do
    it "gives Nothing when given a non-letter" $ do 
      forAll (elements nonletters) $ \c -> isNothing $ fromChar c
    it "gives Just (0-25) when given a letter" $ do
      forAll (elements letters) $ \c -> isJust $ fromChar c
    it "maps letters to their corresponding number (0 = 'A', ...)" $ do 
      mapMaybe fromChar ['A'..'Z'] `shouldBe` map letter [0..25]

  describe "toChar" $ do
    it "is the inverse of fromChar" $ do
      forAll (elements letters) $ \c -> toUpper c == toChar (fromJust $ fromChar c)

  describe "shift" $ do
    context "shifts a letter any number of places" $ do
      it "forward" $ do
        toChar (shift 1 $ letter 1) `shouldBe` 'C'
      it "backwards" $ do     
        toChar (shift (-1) $ letter 1) `shouldBe` 'A'
      it "wrapping around if necessary" $ do
        toChar (shift 26 $ letter 1) `shouldBe` 'B'