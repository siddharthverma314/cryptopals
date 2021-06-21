module Main where

import Convert
import Xor

main :: IO ()
main = do
  let a = decHex "1c0111001f010100061a024b53535009181c"
  let b = decHex "686974207468652062756c6c277320657965"
  let c = xor <$> a <*> b >>= encHex
  putStrLn $ show c
