module AMachine where

import qualified Data.Sequence as S

import qualified AMachine.DOM as DOM
import qualified AMachine.Memory as Memory
import qualified AMachine.Var as Var
import qualified AMachine.Event as E

data MachineState = MachineState
    { environment    :: [Var.Var]
    , domTree        :: DOM.DOMTree
    , memory         :: Memory.Memory
    , pc             :: Int
    , isDOMUpdated   :: Bool
    , isShuttingDown :: Bool
    , evqueue        :: S.Seq E.Event }

init :: MachineState
init = MachineState 
    { environment = []
    , domTree = DOM.init
    , memory = Memory.init
    , pc = 0
    , isDOMUpdated = True
    , isShuttingDown = False
    , evqueue = S.empty }

putEvent :: MachineState -> E.Event -> MachineState
putEvent st ev =
    st { evqueue = ev S.<| (evqueue st) }

tick :: MachineState -> MachineState
tick st =
    let code = Memory.fetch (pc st) $ memory st in
    exec st code

exec :: MachineState -> Memory.Instr -> MachineState
exec st Memory.IWait
    | not $ S.null $ evqueue st = st { pc = (pc st) + 1}
    | otherwise = st
exec st Memory.IShutdown = st { isShuttingDown = True }

test = AMachine.init { domTree = DOM.testTree, memory = Memory.test }