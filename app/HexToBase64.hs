module HexToBase64 where

import Cryptopals.Convert
import Data.Maybe (fromJust)

str = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

main :: IO ()
main = putStrLn $ fromJust $ do
  hex <- decHex str
  encBase64 hex
