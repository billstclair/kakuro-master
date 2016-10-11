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
import Text.Printf
import Debug.Trace (traceShow)

data Limits = Limits { maxVolume :: Int
                     , maxBook :: Int
                     , maxNumber :: Int
                     } deriving (Show, Read)

defaultLimits :: Limits
defaultLimits =
  Limits { maxVolume = 9
         , maxBook = 100
         , maxNumber = 8
         }

limits :: [(Int, Limits)]
limits =
  [ (6, defaultLimits)
  , (8, defaultLimits)
  , (10, defaultLimits)
  ]

data PuzzleKind = Kind6 | Kind8 | Kind10
                deriving (Eq, Show, Read)

kindNumber :: PuzzleKind -> Int
kindNumber Kind6 = 6
kindNumber Kind8 = 8
kindNumber Kind10 = 10

nextKind :: PuzzleKind -> PuzzleKind
nextKind Kind6 = Kind8
nextKind Kind8 = Kind10
nextKind Kind10 = Kind6

data Puzzle = Puzzle { puzzleKind :: PuzzleKind
                     , puzzleVolume :: Int
                     , puzzleBook :: Int
                     , puzzleNumber :: Int
                     } deriving (Eq, Show, Read)

krazydadBaseUrl :: String
krazydadBaseUrl = "http://krazydad.com/tablet/kakuro/"
--krazydadBaseUrl = "https://billstclair.com/kdpage.html"

url :: Puzzle -> String
url puz =
  krazydadBaseUrl ++ "?kind=" ++ (show $ kindNumber $ puzzleKind puz)
  ++ "&volumeNumber=" ++ (show $ puzzleVolume puz)
  ++ "&bookNumber=" ++ (show $ puzzleBook puz)
  ++ "&puzzleNumber=" ++ (show $ puzzleNumber puz)

firstPuzzle :: Puzzle
firstPuzzle = Puzzle Kind6 1 1 1

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

getPuzzleSpec :: Puzzle -> IO String
getPuzzleSpec puz =
  do
    r <- get $ url puz
    let body = r ^. responseBody
      in
      return $ findPuzzLine body

gps = getPuzzleSpec firstPuzzle

nextVolume :: Int -> Int
nextVolume volume =
  if volume == (maxVolume defaultLimits) then 1 else volume + 1

nextBook :: Int -> Int
nextBook book =
  if book == (maxBook defaultLimits) then 1 else book + 1

nextNumber :: Int -> Int
nextNumber number =
  if number == (maxNumber defaultLimits) then 1 else number + 1

nextPuzzle :: Puzzle -> Puzzle
nextPuzzle puzzle =
  let (Puzzle kind volume book number) = puzzle
      newKind = nextKind kind
      newVolume = if newKind == Kind6 then nextVolume volume else volume
      newBook = if (newKind==Kind6 && newVolume==1) then
                  nextBook book else book
      newNumber = if (newKind==Kind6 && newVolume==1 && newBook==1) then
                    nextNumber number else number      
      in
      Puzzle newKind newVolume newBook newNumber

putPuzzleSpecs :: Int -> Puzzle -> IO Puzzle
putPuzzleSpecs count puzzle =
  if count <= 0 then
    do return puzzle
  else do
    spec <- getPuzzleSpec puzzle
    let Puzzle kind volume book number = puzzle in
      printf
        "(%d, %d, %d, %d, \"%s\")\n"
        (kindNumber $ puzzleKind puzzle)
        (puzzleVolume puzzle)
        (puzzleBook puzzle)
        (puzzleNumber puzzle)
        spec
    let next = nextPuzzle puzzle in
      if count == 1 || next == firstPuzzle then
        return next
      else
        putPuzzleSpecs (count-1) next

main =
  do
    res <- gps
    putStrLn res
