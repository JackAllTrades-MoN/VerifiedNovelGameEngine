module AMachine where

import Data.Text (Text)
import qualified Data.Sequence as S
import qualified Data.Maybe as Maybe

import qualified AMachine.DOM as DOM
import qualified AMachine.Memory as Memory
import qualified AMachine.Var as Var
import qualified AMachine.Event as E
import qualified AMachine.Instr as I

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
putEvent st E.EClose = st { isShuttingDown = True }
putEvent st ev =
    st { evqueue = evqueue st S.|> ev }

tick :: MachineState -> MachineState
tick st =
    let code = Memory.fetch (pc st) $ memory st in
    exec st code

exec :: MachineState -> I.Instr -> MachineState
exec st I.IWait
    | not $ S.null $ evqueue st = st { pc = pc st + 1
                                     , evqueue = S.drop 1 $ evqueue st }
    | otherwise = st
exec st I.IShutdown = st { isShuttingDown = True }
exec st attr @ I.IAddPicture {} =
    let dom = DOM.DOMTree
              { DOM.domid   = I.domid attr
              , DOM.name    = I.domid attr
              , DOM.element = DOM.EPicture
                                { DOM.src = I.src attr
                                , DOM.position =
                                      Maybe.fromMaybe (0, 0) $
                                       I.position attr
                                , DOM.w = I.w attr
                                , DOM.h = I.h attr }
              , DOM.children = []}
    in st { pc = pc st + 1
          , isDOMUpdated = True
          , domTree = DOM.addChildTo (I.targetId attr) dom (domTree st) }

exec st attr @ I.IAddText {} =
    let dom = DOM.DOMTree
              { DOM.domid   = I.domid attr
              , DOM.name    = I.domid attr
              , DOM.element = DOM.EText
                                { DOM.value    = I.value attr
                                , DOM.fontSize =
                                    Maybe.fromMaybe 20 $ I.fontSize attr
                                , DOM.color    =
                                    Maybe.fromMaybe (0, 0, 0, 0) $
                                     I.color attr 
                                , DOM.position =
                                      Maybe.fromMaybe (0, 0) $
                                       I.position attr }
              , DOM.children = []}
    in st { pc = pc st + 1
          , isDOMUpdated = True
          , domTree = DOM.addChildTo (I.targetId attr) dom (domTree st) }

loadInstr :: FilePath -> MachineState -> IO MachineState
loadInstr path st = do
    str <- readFile path
    let insts = read str :: [I.Instr]
    return $ st { memory = Memory.fromList insts }

{-
test :: IO MachineState
test = do
    mem <- Memory.load "test/insts.txt"
    return $ AMachine.init { domTree = DOM.init, memory = mem }
    -}