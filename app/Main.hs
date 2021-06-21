{-# LANGUAGE TypeApplications #-}
module Main where

import qualified Data.Map as M
import qualified HexToBase64 as S1C1

apps :: M.Map (Int, Int) (IO ())
apps =
  M.fromList
    [ ((1, 1), S1C1.main)
    ]

main :: IO ()
main = do
  putStrLn "(set)>> "
  set <- read @Int <$> getLine
  putStrLn "(challenge)>> "
  challenge <- read @Int <$> getLine
  let app = apps M.! (set, challenge)
  app
