module Cipher.Polyalphabetic where

import           Cipher (Key, Message, tabulaRecta,
                         tabulaRectaInv)

vigenere :: Key -> Message -> Message
vigenere = zipWith tabulaRecta . cycle

unvigenere :: Key -> Message -> Message
unvigenere = zipWith tabulaRectaInv . cycle
