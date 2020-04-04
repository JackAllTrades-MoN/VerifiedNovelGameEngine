module AMachine where

import AMachine.DOM as DOM
import AMachine.Memory as Memory
import AMachine.Var as Var

data MachineState = MachineState
    { environment  :: [Var.Var]
    , domTree      :: DOM.DOMTree
    , memory       :: Memory.Memory
    , pc           :: Int
    , isDOMUpdated :: Bool }

init :: MachineState
init = MachineState 
    { environment = []
    , domTree = DOM.init
    , memory = Memory.init
    , pc = 0
    , isDOMUpdated = True }

test = AMachine.init { domTree = DOM.testTree }

