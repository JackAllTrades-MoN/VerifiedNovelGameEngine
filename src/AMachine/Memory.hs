{-# LANGUAGE OverloadedStrings #-}
module AMachine.Memory where

import qualified Data.Array as A
import qualified AMachine.Instr as I

newtype Memory = Memory (A.Array Int I.Instr)

init :: Memory
init = Memory $ A.array (0,0) []

fromList :: [I.Instr] -> Memory
fromList insts =
    let idx   = [0..length insts] in
    Memory $ A.array (0, length insts) $ zip idx insts

fetch :: Int -> Memory -> I.Instr
fetch ix (Memory mem) = mem A.! ix

