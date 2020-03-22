module Cipher.Substitution where

import           Cipher     (Message, letter, numberOfLetters, shift, shiftWith,
                             unLetter)
import           Data.List  (elemIndex)
import           Data.Maybe (fromJust)

-- CAESAR
caesar :: Int -> Message -> Message
caesar = map . shift

uncaesar :: Int -> Message -> Message
uncaesar = caesar . negate

-- ATBASH
atbash :: Message -> Message
atbash = map $ (mirror !!) . unLetter
  where
    mirror = map letter $ reverse [0 .. numberOfLetters - 1]

unatbash :: Message -> Message
unatbash = atbash

-- AFFINE
newtype AlphabetCoprime =
  AlphabetCoprime Int
  deriving (Show)

alphabetCoprime :: Int -> Maybe AlphabetCoprime
alphabetCoprime a
  | gcd a numberOfLetters == 1 = Just $ AlphabetCoprime a
  | otherwise = Nothing

affine :: AlphabetCoprime -> Int -> Message -> Message
affine (AlphabetCoprime a) b = map . shiftWith $ \x -> a * x + b

unaffine :: AlphabetCoprime -> Int -> Message -> Message
unaffine (AlphabetCoprime a) b = map . shiftWith $ \x -> aInv * (x - b)
  where
    aInv = modInv a numberOfLetters
    modInv a' m' =
      fromJust . elemIndex 1 $ map (\n -> a' * n `mod` m') [0 .. m' - 1]
