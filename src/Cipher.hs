module Cipher
  ( Letter
  , letter
  , unLetter
  , fromString
  , fromChar
  , toString
  , toChar
  , shift
  , caesar
  , uncaesar
  , vigenere
  , unvigenere
  ) where

import           Data.Char
import           Data.Maybe

newtype Letter =
  Letter Int
  deriving (Eq, Ord)

instance Enum Letter where
  toEnum = letter
  fromEnum = unLetter

instance Show Letter where
  show = show . toChar

type Message = [Letter]

type Key = [Letter]

-- (DE)CONSTRUCTORS
letter :: Int -> Letter
letter i = Letter $ i `mod` 26

unLetter :: Letter -> Int
unLetter (Letter lt) = lt

-- UTILS
fromString :: String -> Message
fromString = mapMaybe fromChar

fromChar :: Char -> Maybe Letter
fromChar ch
  | ch `elem` letters = Just . letter $ ord (toLower ch) - ord 'a'
  | otherwise = Nothing
  where
    letters = ['A' .. 'Z'] ++ ['a' .. 'z']

toString :: Message -> String
toString = map toChar

toChar :: Letter -> Char
toChar = chr . (+ ord 'a') . unLetter

shift :: Int -> Letter -> Letter
shift x lt = letter $ x + unLetter lt

-- CIPHERS
caesar :: Int -> Message -> Message
caesar = map . shift

uncaesar :: Int -> Message -> Message
uncaesar = caesar . negate 

vigenere :: Key -> Message -> Message
vigenere = zipWith f . cycle
  where
    f k = shift $ unLetter k

unvigenere :: Key -> Message -> Message
unvigenere = vigenere . map (letter . negate . unLetter)
