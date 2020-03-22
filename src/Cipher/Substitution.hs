module Cipher.Substitution where

import           Cipher     (Key, Letter, Message, letter, numberOfLetters,
                             shift, shiftWith, tabulaRecta, tabulaRectaInv,
                             unLetter)
import qualified Data.DList as DL
import           Data.List  (elemIndex)
import           Data.Maybe (fromJust)

allLetters :: [Letter]
allLetters = map letter [0 .. numberOfLetters - 1]

-- CAESAR
caesar :: Int -> Message -> Message
caesar = map . shift

uncaesar :: Int -> Message -> Message
uncaesar = caesar . negate

-- ATBASH
atbash :: Message -> Message
atbash = map $ (reverse allLetters !!) . unLetter

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

-- AUTOKEY
autokey :: Key -> Message -> Message
autokey key mess = zipWith tabulaRecta (key ++ mess) mess

unautokey :: Key -> Message -> Message
unautokey [] _ = []
unautokey key mess = go mess (DL.fromList key) DL.empty
  where
    go [] _ res = DL.toList res
    go (m:ms) ks res =
      let dech = tabulaRectaInv (DL.head ks) m
       in go ms (DL.tail ks `DL.snoc` dech) (res `DL.snoc` dech)
