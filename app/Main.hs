{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import SDL
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

import Monitor
import AMachine

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
  window <- createWindow "VeNGE" SDL.defaultWindow { windowInitialSize = V2 800 600 }
  renderer <- SDL.createRenderer window (-1) rdrConfig 
  let col = SDL.rendererDrawColor renderer
  col $= black
  SDL.showWindow window
  appLoop renderer AMachine.test
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.Font.quit
  SDL.quit

appLoop :: SDL.Renderer -> AMachine.MachineState -> IO ()
appLoop renderer st = do
  ev <- SDL.pollEvents
  let (Any quit, Sum ct) =
        foldMap ((\case
          SDL.QuitEvent -> (Any True, Sum 0)
          KeyboardEvent kev 
            | keyboardEventKeyMotion kev == Pressed &&
              keysymKeycode (keyboardEventKeysym kev) == KeycodeQ
            -> (Any True, Sum 0) 
          _ -> (Any False, Sum 0)) . SDL.eventPayload) ev
  Monitor.printScreen renderer st
  unless quit $ appLoop renderer st { isDOMUpdated = False }

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
