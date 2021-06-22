module Solutions.BreakXorSingle where

import Cryptopals.Xor
import Cryptopals.Convert
import Data.Maybe (fromJust)

inp = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

main :: IO ()
main = do
  let decWords = convert 8 $ fromJust $ decHex inp
  let dec = wordArr $ decWords
  let key = breakXorSingle dec
  putStrLn $ toEnum <$> singleXor key dec

