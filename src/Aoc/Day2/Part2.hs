module Aoc.Day2.Part2 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Aoc.Day2.Part1

solve :: [BS.ByteString] -> String
solve (x:xs) = show $ 100 * (fst findSuccess) + snd findSuccess
  where
    findSuccess = fromJust $ find predicate getAllAttempts
    predicate = (==) 19690720 . getResult
    getResult (noun, verb) = fromJust . Map.lookup 0 . iterateIndex 0 $ getAdjustedOpcodeMap' noun verb x

getAdjustedOpcodeMap' :: Int -> Int -> BS.ByteString -> Map.Map Int Int
getAdjustedOpcodeMap' noun verb = Map.insert 1 noun . Map.insert 2 verb . getOpcodeMap

getAllAttempts :: [(Int, Int)]
getAllAttempts = [(x, y) | x <- [0..99], y <- [0..99]]
