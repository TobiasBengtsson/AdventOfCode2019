module Aoc.Day1.Part1 where

import Data.ByteString
import Data.ByteString.Char8

solve :: [ByteString] -> String
solve lines = show $ sum $ fmap fuelRequired $ intLines lines

intLines :: [ByteString] -> [Int]
intLines = fmap (maybe 0 fst . readInt)

fuelRequired :: Int -> Int
fuelRequired m = (m `div` 3) - 2
