{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE LambdaCase #-}

module Main where

import qualified SDL
import SDL.Raw (renderCopy, Rect(..))
import SDL.Font
import SDL.Video
import SDL.Video.Renderer

import Lib
import Options.Applicative
import Data.Semigroup ((<>), Any(..), Sum(..))
import VConfig
import Data.Text
import Control.Monad
import Control.Monad.State
import qualified Data.List as List
import qualified Data.Sequence as S

import qualified Monitor
import qualified AMachine as AM
import qualified AMachine.Event as AME

red :: SDL.Font.Color
red = SDL.V4 255 0 0 0
black = SDL.V4 0 0 0 0

buildOptions :: Parser Options
buildOptions = pure $ Options Build

runOptions :: Parser Options
runOptions = pure $ Options Run

debugOptions :: Parser Options
debugOptions = pure $ Options VConfig.Debug

parser :: Parser Options
parser =
  subparser
  ( command "build" (info buildOptions (progDesc "to run a novel game script"))
    <> command "run" (info runOptions (progDesc "to make a product build"))
    <> command "debug" (info debugOptions (progDesc "only for development")))

run :: () -> IO ()
run _ = putStrLn "RUUUUUUN"

build :: () -> IO ()
build _ = putStrLn "BUIIIILD"

rdrConfig = SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedRenderer }

debug :: () -> IO ()
debug () = do
  SDL.initialize [SDL.InitVideo]
  SDL.Font.initialize
  window <- createWindow "VeNGE" SDL.defaultWindow { windowInitialSize = SDL.V2 800 600 }
  renderer <- SDL.createRenderer window (-1) rdrConfig 
  let col = SDL.rendererDrawColor renderer
  col SDL.$= black
  SDL.showWindow window
  st <- AM.loadInstr "test/insts.txt" AM.init
  execStateT (appLoop renderer) st
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.Font.quit
  SDL.quit

loadEvent :: SDL.Event-> StateT AM.MachineState IO ()
loadEvent ev = do
  st <- get
  case SDL.eventPayload ev of
    SDL.KeyboardEvent kev
     | SDL.keyboardEventKeyMotion kev == SDL.Pressed &&
      SDL.keysymKeycode (SDL.keyboardEventKeysym kev) == SDL.KeycodeQ
      -> put $ AM.putEvent st $ AME.EKey "Q"
    SDL.WindowClosedEvent wev
      -> put $ AM.putEvent st AME.EClose
    _ -> return ()

tick :: StateT AM.MachineState IO ()
tick = do
  st <- get
  put $ AM.tick st

printScreen :: SDL.Renderer -> StateT AM.MachineState IO ()
printScreen renderer = do
  st <- get
  lift $ Monitor.printScreen renderer st
  put $ st { AM.isDOMUpdated = False }

appLoop :: SDL.Renderer -> StateT AM.MachineState IO ()
appLoop renderer = do
  evs <- lift SDL.pollEvents
  mapM_ loadEvent evs
  tick
  printScreen renderer
  st <- get
--  lift $ print $ S.length $ AM.evqueue st
  unless (AM.isShuttingDown st) $ appLoop renderer 

main :: IO ()
main =
  execParser opts >>= \opts ->
  driver $ optCommand opts
  where
    opts = info (parser <**> helper)
           (fullDesc
            <> progDesc ""
            <> header "VeNGE a Verified Novel Game Engine")
    driver Build = build ()
    driver Run = run ()
    driver VConfig.Debug = debug ()
