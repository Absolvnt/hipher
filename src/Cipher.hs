{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cipher where

import           Data.Char
import           Data.Maybe

newtype Letter =
  Letter Int
  deriving (Num, Real, Integral)

instance Eq Letter where
    Letter x == Letter y = (x `mod` 26) == (y `mod` 26)

instance Ord Letter where
    compare (Letter x) (Letter y) = compare (x `mod` 26) (y `mod` 26)

instance Enum Letter where
    toEnum = Letter
    fromEnum (Letter x) = x `mod` 26

instance Show Letter where
  show = show . toChar

type Message = [Letter]

type Key = [Letter]

-- UTILS

fromString :: String -> Message
fromString = mapMaybe fromChar

fromChar :: Char -> Maybe Letter
fromChar ch
  | ch `elem` letters = Just . Letter $ ord (toLower ch) - ord 'a'
  | otherwise = Nothing
  where letters = ['A' .. 'Z'] ++ ['a' .. 'z']

toString :: Message -> String
toString = map toChar

toChar :: Letter -> Char
toChar (Letter lt) = chr $ lt `mod` 26 + ord 'a'

shift :: Int -> Letter -> Letter
shift = (+) . Letter

-- CIPHERS
caesar :: Int -> Message -> Message
caesar = map . shift

uncaesar :: Int -> Message -> Message
uncaesar = caesar . negate

vigenere :: Key -> Message -> Message
vigenere = zipWith f . cycle
  where
    f (Letter k) = shift k

unvigenere :: Key -> Message -> Message
unvigenere = vigenere . map negate