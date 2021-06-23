{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Solutions.DetectXorSingle where

import Control.Monad (forM_)
import Cryptopals.Convert
import Cryptopals.Xor
  ( alphaSort,
    breakXorSingle,
    englishFreq,
    kld,
    lstToFreq,
    singleXor,
  )
import qualified Data.ByteString.Char8 as B
import Data.List (sortOn)
import Data.Maybe
import Data.Ord ()
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Req
  ( GET (GET),
    NoReqBody (NoReqBody),
    bsResponse,
    defaultHttpConfig,
    https,
    req,
    responseBody,
    runReq,
    (/:),
  )

main :: IO ()
main = do
  -- fetch the challenge text
  putStrLn "Fetching challenge"
  req <-
    responseBody
      <$> runReq
        defaultHttpConfig
        ( req
            GET
            (https "cryptopals.com" /: "static" /: "challenge-data" /: "4.txt")
            NoReqBody
            bsResponse
            mempty
        )
  let lines = B.unpack <$> B.split '\n' req

  -- decode each line
  putStrLn "Decoding"
  let decoded =
        do
          l' <- lines
          let l = wordArr $ convert 8 $ fromJust $ decHex l'
          let k = breakXorSingle l
          return $ singleXor k l

  -- identify sentences which have distribution similar to english
  putStrLn "Sorting"
  let candidates =
        let englishKld = flip kld (alphaSort englishFreq)
         in sortOn (englishKld . lstToFreq) decoded
  let sents = do
        sent <- candidates
        return $ toEnum @Char <$> sent

  -- print top 3 sentences
  forM_ (take 10 sents) putStrLn
