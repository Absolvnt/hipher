module CipherSpec where

import           Cipher
import           Data.Maybe
import           Test.Hspec
import           Test.QuickCheck
import Data.List ((\\))
import Data.Char (toLower)

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
      it "displays the lower-case alphabet for 0-25" $ do 
          forAll (elements [0..25]) $ \x -> show (letter x) == show (['a'..'z'] !! x)
      it "displays the same character given a value modulo 26" $ do
        property $ \x m -> show (letter x) == show (letter $ x + 26*m)
    
  describe "fromChar" $ do
    it "gives Nothing when given a non-letter" $ do 
      forAll (elements nonletters) $ \c -> isNothing $ fromChar c
    it "gives Just (0-25) when given a letter" $ do
      forAll (elements letters) $ \c -> isJust $ fromChar c
    it "maps letters to their corresponding number (0 = 'a', ...)" $ do 
      mapMaybe fromChar ['a'..'z'] `shouldBe` map letter [0..25]

  describe "fromString" $ do
    it "is just a mappedMaybe version of fromChar (empty test)" $ do
      True 

  describe "toChar" $ do
    it "is the inverse of fromChar" $ do
      forAll (elements letters) $ \c -> toLower c == toChar (fromJust $ fromChar c)

  describe "toString" $ do
    it "is just a mapped version of toChar (empty test)" $ do
      True 

  describe "shift" $ do
    context "shifts a letter any number of places" $ do
      it "forward" $ do
        toChar (shift 1 $ letter 1) `shouldBe` 'c'
      it "backwards" $ do     
        toChar (shift (-1) $ letter 1) `shouldBe` 'a'
      it "wrapping around if necessary" $ do
        toChar (shift 26 $ letter 1) `shouldBe` 'b'

-- TODO: Add some more rigorous testing for the ciphers

  describe "caesar" $ do
    it "applies a Caeser cipher" $ do
      toString (caesar 25 $ fromString "helloworld") `shouldBe` "gdkknvnqkc" 

  describe "uncaesar" $ do
    it "is the inverse (decipher) of caesar" $ do
      toString (uncaesar 25 $ fromString "gdkknvnqkc") `shouldBe` "helloworld" 

  describe "vigenere" $ do
    it "applies a Viginere cipher" $ do
      toString (vigenere (fromString "hask") $ fromString "helloworld") `shouldBe` "oedvvwgbsd"

  describe "unvigenere" $ do
    it "is the inverse (decipher) of vigenere" $ do
      toString (unvigenere (fromString "hask") $ fromString "oedvvwgbsd") `shouldBe` "helloworld"  