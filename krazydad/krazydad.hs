--
-- krazydad.hs
--
-- Run with:
--   runhaskell krazydad >output-file
--
-- Or, in ghci:
--   :l krazydad
--   getPuzzleSpec $ Puzzle 6 1 1 1
--

import Network.Wreq (get, responseBody)
import Control.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Debug.Trace (traceShow)

data Limits = Limits { maxVolume :: Int
                     , maxBook :: Int
                     , maxPuzzle :: Int
                     } deriving (Show)

defaultLimits :: Limits
defaultLimits =
  Limits { maxVolume = 9
         , maxBook = 100
         , maxPuzzle = 8
         }

limits :: [(Int, Limits)]
limits =
  [ (6, defaultLimits)
  , (8, defaultLimits)
  , (10, defaultLimits)
  ]

data Puzzle = Puzzle { kind :: Int
                     , volume :: Int
                     , book :: Int
                     , puzzle :: Int
                     } deriving (Show)

krazydadBaseUrl :: String
--krazydadBaseUrl = "http://krazydad.com/tablet/kakuro/"
krazydadBaseUrl = "https://billstclair.com/kdpage.html"

url :: Puzzle -> String
url puz =
  krazydadBaseUrl ++ "?kind=" ++ (show $ kind puz)
  ++ "&volumeNumber=" ++ (show $ volume puz)
  ++ "&bookNumber=" ++ (show $ book puz)
  ++ "&puzzleNumber=" ++ (show $ puzzle puz)

firstPuzzle :: Puzzle
firstPuzzle = Puzzle 10 1 1 1

puzzColon :: B.ByteString
puzzColon = BC.pack "{\"puzz\": \""

quote :: B.ByteString
quote = BC.pack "\""

extractPuzz :: B.ByteString -> Maybe String
extractPuzz line =
  case B.breakSubstring puzzColon line of
    (x, y) | B.null y -> Nothing
           | otherwise ->
             Just $ BC.unpack $
             fst $ B.breakSubstring quote $ B.drop (B.length puzzColon) y

-- This looks for a line like the following, and returns the "puzz" value:
--  var pRec = {"puzzle_data": {"puzz": "...79..28.12.879.127289136.798..72..59..21.32518964329817.12..35..39..721.214738879.132.21.14..48...", "passes": "127", "cells": "68", 
findPuzzLine :: BL.ByteString -> String
findPuzzLine body =
  let (head, tail) = BL.span (\x -> x /= 10) body
    in
    case extractPuzz $ BL.toStrict head of
      Nothing ->
        if BL.null tail then
          ""
        else
          findPuzzLine $ BL.tail tail
      Just res ->
        res

getPuzzleSpec puz =
  do
    r <- get $ url puz
    let body = r ^. responseBody
      in
      return $ findPuzzLine body

gps = getPuzzleSpec firstPuzzle

main =
  do
    res <- gps
    putStrLn res
