module AMachine.Memory where

import qualified Data.Array as A

data Instr = IWait | IShutdown

newtype Memory = Memory (A.Array Int Instr)

init :: Memory
init = Memory $ A.array (0,0) []


test :: Memory
test = Memory $ A.array (0, 2) [(0, IWait), (1, IShutdown)]

fetch :: Int -> Memory -> Instr
fetch ix (Memory mem) = mem A.! ix

