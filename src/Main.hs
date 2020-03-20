module Main where

import           Cipher
import           Data.Char
import           System.IO
import           Text.Read

dispatch :: [(String, IO String)]
dispatch = [("caesar", caesarIO), ("vigenere", vigenereIO)]

askInt :: String -> IO Int
askInt p = do
  putStrLn p
  int' <- getLine
  case readMaybe int' of
    Just int -> return int
    Nothing  -> askInt "Invalid integer input."

askStr :: String -> IO String
askStr p = putStrLn p >> getLine

caesarIO :: IO String
caesarIO = do
  int <- askInt "Input substitution step (int): "
  mess <- askStr "Input message to cipher: "
  return . toString $ caesar int (fromString mess)

vigenereIO :: IO String
vigenereIO = do
  key <- askStr "Input keyword: "
  mess <- askStr "Input message to cipher: "
  return . toString $ vigenere (fromString key) (fromString mess)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  requestedCipher <- askStr "Pick cipher: "
  case lookup requestedCipher dispatch of
    Just cio -> cio >>= putStrLn
    Nothing  -> putStrLn "Cipher not found." >> main
