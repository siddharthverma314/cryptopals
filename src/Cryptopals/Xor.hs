{-# LANGUAGE TypeApplications #-}

module Cryptopals.Xor where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Bits (Bits, xor)
import Data.Char (toUpper)
import Data.List (group, minimumBy, sort)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.STRef (modifySTRef, newSTRef, readSTRef)

-- | Code pad encoding
fixedXor :: Bits a => [a] -> [a] -> [a]
fixedXor = zipWith xor

-- | Frequencies of letters in the English alphabet
englishFreq :: M.Map Char Double
englishFreq =
  M.fromList
    [ ('E', 0.120195499),
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
      ('Z', 7.0212750e-4)
    ]

-- | XOR a stream against a single letter
singleXor :: Bits a => a -> [a] -> [a]
singleXor x = fixedXor (repeat x)

-- | Compute the KL divergence between two distributions
kld :: (Eq a, Floating a) => [a] -> [a] -> a
kld ps qs = mean $ zipWith (\p q -> p * log0 p q) (norm ps) (norm qs)
  where
    norm xs = (/ sum xs) <$> xs
    log0 p q
      | p == 0 = 0
      | q == 0 = error "KL divergence not defined for q=0"
      | otherwise = log (p / q)
    mean x = sum x / fromIntegral (length x)

-- | Group elements by frequency
freq :: (Ord a, Floating b) => [a] -> M.Map a b
freq xs = runST $ do
  m <- newSTRef M.empty

  -- sum up counts
  forM_ xs $ \x -> do
    m' <- readSTRef m
    modifySTRef
      m
      ( \m' ->
          let count = fromMaybe 0 $ m' M.!? x
           in M.insert x (count + 1) m'
      )

  -- return
  readSTRef m

-- | Given a frequency mapping, return the frequency of A-Z as a list
alphaSort :: M.Map Char Double -> [Double]
alphaSort m = do
  c <- ['A' .. 'Z']
  return $ fromMaybe 0 $ m M.!? c

-- | Given a list, return the distribution of character frequency
lstToFreq :: [Int] -> [Double]
lstToFreq = map (+ 0.01) . alphaSort . freq . map (toUpper . toEnum)

-- | Crack single character XOR encryption. Works by trying all
-- combinations and picking the one with the lowest KL divergence from
-- the frequency of the English alphabet
breakXorSingle :: [Int] -> Int
breakXorSingle xs = minimumBy (comparing kldFreq) [0 .. 255]
  where
    kldFreq :: Int -> Double
    kldFreq key = kld (lstToFreq $ singleXor key xs) (alphaSort englishFreq)
