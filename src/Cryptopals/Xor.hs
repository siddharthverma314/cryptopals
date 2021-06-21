module Cryptopals.Xor where

import Data.Bits (xor)
import Data.Map (Map)
import qualified Data.Map as Map

-- fixed xor
fixedXor :: [Int] -> [Int] -> [Int]
fixedXor = zipWith xor

-- single byte xor
frequencies :: Map Char Double
frequencies =
  Map.fromList
    [ ('E', 0.12019550),
      ('T', 0.090985886),
      ('A', 0.081238378),
      ('O', 0.076811682),
      ('I', 0.073054201),
      ('N', 0.069477738),
      ('S', 0.062807524),
      ('R', 0.060212942),
      ('H', 0.059214604),
      ('D', 0.043191829),
      ('L', 0.039785412),
      ('U', 0.028776268),
      ('C', 0.027114200),
      ('M', 0.026115862),
      ('F', 0.023038568),
      ('Y', 0.021135143),
      ('W', 0.020948640),
      ('G', 0.020257483),
      ('P', 0.018189498),
      ('B', 0.014892788),
      ('V', 0.011074969),
      ('K', 6.8951142e-3),
      ('X', 1.7278926e-3),
      ('Q', 1.1245015e-3),
      ('J', 1.0312502e-3),
      ('Z', 7.0212778e-4)
    ]

singleXor :: Int -> [Int] -> Int
singleXor a = undefined $ fixedXor (repeat a)

breakXorSingle :: [Int] -> Int
breakXorSingle = undefined
  where
    combinations = zip [0 .. 255]
