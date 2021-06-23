{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Map as M
import qualified Solutions.BreakXorSingle as S1C3
import qualified Solutions.DetectXorSingle as S1C4
import qualified Solutions.HexToBase64 as S1C1
import qualified Solutions.Xor as S1C2
import System.IO (hFlush, stdout)

apps :: M.Map (Int, Int) (IO ())
apps =
  M.fromList
    [ ((1, 1), S1C1.main),
      ((1, 2), S1C2.main),
      ((1, 3), S1C3.main),
      ((1, 4), S1C4.main)
    ]

main :: IO ()
main = do
  putStr "(set)>> "
  hFlush stdout
  set <- read @Int <$> getLine

  putStr "(challenge)>> "
  hFlush stdout
  challenge <- read @Int <$> getLine

  case apps M.!? (set, challenge) of
    Just app -> app
    Nothing -> putStrLn "Set/Challenge solution does not exist!"
