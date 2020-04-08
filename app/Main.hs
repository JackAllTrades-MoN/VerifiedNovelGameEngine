{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE LambdaCase #-}

module Main where

import qualified SDL
import qualified SDL.Font
import qualified SDL.Video
import qualified SDL.Video.Renderer

import Options.Applicative
import Data.Semigroup ((<>), Any(..), Sum(..))
import VConfig
import Data.Text (Text, unpack)
import Control.Monad
import Control.Monad.State
import qualified Data.List as List
import qualified Data.Sequence as S
import qualified System.Process as Process

import Data.FileEmbed (embedFile, makeRelativeToProject)
import qualified Data.ByteString as BS

import qualified Game
import qualified Sample

build :: () -> IO ()
build () = do
  excode <- Process.system "mkdir build"
  _ <- Process.system "cp driver/StubMain.hs build/Main.hs"
  _ <- Process.system "cp src/Game.hs build/Game.hs"
  _ <- Process.system "cp src/Sample.hs build/Sample.hs"
  _ <- Process.system "cp -r src/Component build/"
  _ <- Process.system "cd build; stack ghc -- Game.hs Main.hs"
  putStrLn "Build directory"

sample :: () -> IO ()
sample () = do
  let rdrConfig = SDL.defaultRenderer
                  { SDL.rendererType = SDL.AcceleratedRenderer }
      black     = SDL.V4 0 0 0 0 :: SDL.Font.Color 
  SDL.initialize [SDL.InitVideo]
  SDL.Font.initialize
  window <- SDL.createWindow "VeNGE" SDL.defaultWindow { SDL.Video.windowInitialSize = SDL.V2 800 600 }
  renderer <- SDL.createRenderer window (-1) rdrConfig
  let col = SDL.rendererDrawColor renderer
  col SDL.$= black
  SDL.showWindow window
  Game.runGame_ Sample.title renderer
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.Font.quit
  SDL.quit

stubMain :: BS.ByteString
stubMain = $(makeRelativeToProject "resources/StubMain.hs" >>= embedFile)

autoGenReadMe :: BS.ByteString
autoGenReadMe = $(makeRelativeToProject "resources/readme.txt" >>= embedFile)

new :: Text -> IO ()
new projectName = do
  let dir = "projects/" <> projectName
  Process.system $ unpack $ "mkdir " <> dir
  Process.system $ unpack $ "cd " <> dir <> "; mkdir src build img font bin"
  BS.writeFile (unpack $ dir <> "/src/Main.hs") stubMain
  BS.writeFile (unpack $ dir <> "/readme.txt") autoGenReadMe
  putStrLn "DONE"

main :: IO ()
main =
  execParser VConfig.parseInfo >>= run
  where
    run Build = build ()
    run Sample = sample ()
    run (New projectName) = new projectName