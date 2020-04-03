{-# LANGUAGE OverloadedStrings #-}

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

red :: SDL.Font.Color
red = SDL.V4 255 0 0 0
black = SDL.V4 0 0 0 0

{-
printMsg :: SDL.Window -> SDL.Font.Color -> Int -> Text -> IO ()
printMsg window color size msg = do
  font <- SDL.Font.load "font/mplus-1p-regular.ttf" size
  text <- SDL.Font.solid font color msg -- surface
  SDL.Font.free font
  txtr <- SDL.cr
  screen <- SDL.getWindowSurface window
  SDL.surfaceBlit text Nothing screen Nothing
  SDL.freeSurface text
  SDL.updateWindowSurface window -}

printMsg :: SDL.Renderer -> SDL.Font.Color -> Int -> Text -> IO ()
printMsg renderer color size msg = do
  font <- SDL.Font.load "font/mplus-1p-regular.ttf" size
  surface <- SDL.Font.solid font color msg
  txtr <- SDL.createTextureFromSurface renderer surface
  let rect = SDL.Rectangle (P $ V2 0 0) (V2 100 100)
  SDL.copy renderer txtr Nothing (Just rect)

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
  window <- createWindow "VeNGE" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) rdrConfig 
  let col = SDL.rendererDrawColor renderer
  col $= black
  SDL.showWindow window
  appLoop renderer
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.Font.quit
  SDL.quit

appLoop :: SDL.Renderer -> IO ()
appLoop renderer = do
  ev <- SDL.pollEvents
  let (Any quit, Sum ct) =
        foldMap ((\e -> case e of
          SDL.QuitEvent -> (Any True, Sum 0)
          KeyboardEvent kev 
            | keyboardEventKeyMotion kev == Pressed &&
              keysymKeycode (keyboardEventKeysym kev) == KeycodeQ
            -> (Any True, Sum 0) 
          _ -> (Any False, Sum 0)) . SDL.eventPayload) ev
  SDL.clear renderer
  printMsg renderer red 20 "hoge"
  SDL.present renderer
  unless quit $ appLoop renderer

{-
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
-}

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
