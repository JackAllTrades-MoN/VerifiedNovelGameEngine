{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL
import SDL.Font

import Lib
import Options.Applicative
import Data.Semigroup ((<>))
import VConfig
import Data.Text

red :: SDL.Font.Color
red = SDL.V4 255 0 0 0

printMsg :: SDL.Window -> SDL.Font.Color -> Int -> Text -> IO ()
printMsg window color size msg = do
  font <- SDL.Font.load "font/mplus-1p-regular.ttf" size
  text <- SDL.Font.solid font color msg
  SDL.Font.free font
  screen <- SDL.getWindowSurface window
  SDL.surfaceBlit text Nothing screen Nothing
  SDL.freeSurface text
  SDL.updateWindowSurface window

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

debug :: () -> IO ()
debug () = do
  SDL.initialize [SDL.InitVideo]
  SDL.Font.initialize
  window <- createWindow "VeNGE" SDL.defaultWindow
  SDL.showWindow window
  printMsg window red 20 "hoge"
  appLoop
  SDL.destroyWindow window
  SDL.Font.quit
  SDL.quit
{-  
  initializeAll
  window <- createWindow "VeNGE" WindowConfig {
      windowBorder          = True
    , windowHighDPI         = False
    , windowInputGrabbed    = False
    , windowMode            = Windowed
    , windowGraphicsContext = NoGraphicsContext
    , windowPosition        = Wherever
    , windowResizable       = True
    , windowInitialSize     = V2 800 600
    , windowVisible         = True
  }
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop
-}

appLoop :: IO ()
appLoop = waitEvent >>= go
  where
  go :: Event -> IO ()
  go ev =
    case eventPayload ev of
      KeyboardEvent keyboardEvent
        | keyboardEventKeyMotion keyboardEvent == Pressed &&
          keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
        -> return ()
      _ -> waitEvent >>= go

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
