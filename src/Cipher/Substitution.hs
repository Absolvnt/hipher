module Cipher.Substitution where

import           Cipher (Message, shift)

caesar :: Int -> Message -> Message
caesar = map . shift

uncaesar :: Int -> Message -> Message
uncaesar = caesar . negate

atbash :: Message -> Message
atbash = undefined

unatbash :: Message -> Message
unatbash = atbash