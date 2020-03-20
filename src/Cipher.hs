{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cipher where

import           Data.Char

newtype Letter =
  Letter Int deriving (Eq, Ord, Num, Real, Enum, Integral)

instance Show Letter where
  show = show . toChar

type Message = [Letter]
type Key = [Letter]

-- UTILS

fromString :: String -> Message
fromString = map fromChar . filter isLetter

fromChar :: Char -> Letter
fromChar ch = Letter $ ord (toLower ch) - ord 'a'

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
    where f (Letter k) = shift k 

unvigenere :: Key -> Message -> Message
unvigenere = vigenere . map negate