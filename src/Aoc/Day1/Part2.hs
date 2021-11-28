module Aoc.Day1.Part2 where

import Data.ByteString
import Aoc.Day1.Part1

solve :: [ByteString] -> String
solve lines = show $ sum $ fmap fuelRequired' $ intLines lines

fuelRequired' :: Int -> Int
fuelRequired' m
  | fr <= 0   = 0
  | otherwise = fr + fuelRequired' fr
  where
    fr = fuelRequired m
