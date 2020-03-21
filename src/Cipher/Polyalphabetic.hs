module Cipher.Polyalphabetic where

import           Cipher (Key, Message, letter, shift, unLetter)

vigenere :: Key -> Message -> Message
vigenere = zipWith f . cycle
  where
    f k = shift $ unLetter k

unvigenere :: Key -> Message -> Message
unvigenere = vigenere . map (letter . negate . unLetter)