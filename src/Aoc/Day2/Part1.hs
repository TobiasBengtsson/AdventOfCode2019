module Aoc.Day2.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import Data.Maybe

solve :: [BS.ByteString] -> String
solve (x:xs) = show $ fromJust . Map.lookup 0 . iterateIndex 0 $ getAdjustedOpcodeMap x

getAdjustedOpcodeMap :: BS.ByteString -> Map.Map Int Int
getAdjustedOpcodeMap = Map.insert 1 12 . Map.insert 2 2 . getOpcodeMap

getOpcodeMap :: BS.ByteString -> Map.Map Int Int
getOpcodeMap bs = Map.fromList $ zip [0..] opcodeList
  where
    opcodeList :: [Int]
    opcodeList = fmap (fst . fromJust . BC.readInt) $ BC.split ',' bs

iterateIndex :: Int -> Map.Map Int Int -> Map.Map Int Int
iterateIndex i m = case Map.lookup i m of
    Nothing -> m
    Just 99 -> m
    Just 2 -> iterateIndex (i + 4) $ applyOpcode (*) m
    Just 1 -> iterateIndex (i + 4) $ applyOpcode (+) m
  where
    lookupOffset k = m Map.! (i + k)
    lookup2Offset k = m Map.! (lookupOffset k)
    applyOpcode fn = Map.insert (lookupOffset 3) (fn (lookup2Offset 1) (lookup2Offset 2))
