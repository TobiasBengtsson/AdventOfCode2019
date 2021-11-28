module Aoc.Day2.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import Data.Maybe

solve :: [BS.ByteString] -> String
solve (x:xs) = show $ fromJust . Map.lookup 0 . runProgram 0 $ getAdjustedMemoryMap x

getAdjustedMemoryMap :: BS.ByteString -> Map.Map Int Int
getAdjustedMemoryMap = Map.insert 1 12 . Map.insert 2 2 . getMemoryMap

getMemoryMap :: BS.ByteString -> Map.Map Int Int
getMemoryMap bs = Map.fromList $ zip [0..] opcodeList
  where
    opcodeList :: [Int]
    opcodeList = fmap (fst . fromJust . BC.readInt) $ BC.split ',' bs

runProgram :: Int -> Map.Map Int Int -> Map.Map Int Int
runProgram i m = case Map.lookup i m of
    Nothing -> m
    Just 99 -> m
    Just 2 -> applyOpcodeAndProceed (*)
    Just 1 -> applyOpcodeAndProceed (+)
  where
    lookupOffset k = m Map.! (i + k)
    fstVal = m Map.! (lookupOffset 1)
    sndVal = m Map.! (lookupOffset 2)
    updateIndex = lookupOffset 3
    applyOpcode fn = Map.insert updateIndex (fn fstVal sndVal)
    applyOpcodeAndProceed fn = runProgram (i + 4) $ applyOpcode fn m
