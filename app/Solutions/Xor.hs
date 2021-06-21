module Solutions.Xor where

import Cryptopals.Convert as C
import Cryptopals.Xor
import Data.Maybe (fromJust)

main :: IO ()
main = putStrLn $
  fromJust $ do
    a <- decHex "1c0111001f010100061a024b53535009181c"
    b <- decHex "686974207468652062756c6c277320657965"
    c <- C.words (wordBits a) (fixedXor (wordArr a) (wordArr b))
    encHex c 
