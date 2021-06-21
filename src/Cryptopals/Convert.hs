{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Cryptopals.Convert where

import Data.Bits (shiftR, (.&.))
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Prelude hiding (words)

-- | A word is a generic way of representing a number in a certain
-- base. For example, bytes are Words with `wordBits` set to 8 and
-- hexadecimal is Words with `wordBits` set to 16.
data Words = UnsafeWords
  { wordBits :: Int,
    wordArr :: [Int]
  }
  deriving (Show, Eq)

-- | Constructors for Words. Use in lieu of UnsafeWords
words :: Int -> [Int] -> Maybe Words
words wordBits wordArr =
  if all (< 2 ^ wordBits) wordArr
    then Just $ UnsafeWords {..}
    else Nothing

-- Helper functions
pad :: Int -> a -> [a] -> [a]
pad num val list = take num $ list ++ repeat val

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk num list = take num list : chunk num (drop num list)

wordsToBits :: Words -> [Int]
wordsToBits UnsafeWords {..} = wordToBits =<< wordArr
  where
    wordToBits word = map (getbit word) count
    getbit word i = word `shiftR` i .&. 1
    count = reverse $ take wordBits [0 ..]

wordsFromBits :: Int -> [Int] -> Words
wordsFromBits wordBits bits = UnsafeWords {..}
  where
    wordArr = bitsToWord <$> chunk wordBits bits
    bitsToWord = foldl (\x i -> 2 * x + i) 0 . pad wordBits 0

-- | convert any base to any other base
convert :: Int -> Words -> Words
convert wordBits words = wordsFromBits wordBits $ wordsToBits words

data ConvertMap = ConvertMap
  { mapping :: V.Vector Char,
    bits :: Int,
    padding :: Char
  }
  deriving (Show)

hexConvertMap =
  ConvertMap
    { mapping = V.fromList "0123456789abcdef",
      bits = 4,
      padding = '\0'
    }

base64ConvertMap =
  ConvertMap
    { mapping = V.fromList "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
      bits = 6,
      padding = '='
    }

-- encodings and bits
decode :: ConvertMap -> String -> Maybe Words
decode ConvertMap {..} str = words bits =<< traverse charToWord str
  where
    charToWord char = V.elemIndex char mapping

encode :: ConvertMap -> Words -> Maybe String
encode ConvertMap {..} = traverse (mapping V.!?) . wordArr . convert bits

-- functions to export
encHex = encode hexConvertMap

decHex = decode hexConvertMap

encBase64 = encode base64ConvertMap

decBase64 = decode base64ConvertMap
