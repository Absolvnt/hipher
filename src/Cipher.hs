module Cipher
  ( Letter
  , Message
  , Key
  , letter
  , unLetter
  , fromString
  , fromChar
  , toString
  , toChar
  , shift
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

letter :: Int -> Letter
letter i = Letter $ i `mod` 26

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

-- move into more specific module?
shift :: Int -> Letter -> Letter
shift x lt = letter $ x + unLetter lt
