module Cipher
  ( Letter
  , Message
  , Key
  , letter
  , unLetter
  , numberOfLetters
  , fromString
  , fromChar
  , toString
  , toChar
  , cipher
  , shift
  , shiftWith
  ) where

import           Data.Char  (chr, ord, toLower)
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

numberOfLetters :: Int
numberOfLetters = 26     -- Latin alphabet

letter :: Int -> Letter
letter i = Letter $ i `mod` numberOfLetters

unLetter :: Letter -> Int
unLetter (Letter lt) = lt

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

cipher :: (Message -> Message) -> String -> String
cipher cf = toString . cf . fromString

-- move into more specific module?
shift :: Int -> Letter -> Letter
shift = shiftWith . (+)

shiftWith :: (Int -> Int) -> Letter -> Letter
shiftWith f = letter . f . unLetter
