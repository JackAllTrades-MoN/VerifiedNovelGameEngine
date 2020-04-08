{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Font

import qualified Game
import qualified Sample

main :: IO ()
main = do
    let black = SDL.V4 0 0 0 0 :: SDL.Font.Color
    let rdrConfig = SDL.defaultRenderer
                    { SDL.rendererType = SDL.AcceleratedRenderer }
    SDL.initialize [SDL.InitVideo]
    SDL.Font.initialize
    window <- SDL.createWindow "VeNGE" SDL.defaultWindow
                { SDL.windowInitialSize = SDL.V2 800 600 }
    renderer <- SDL.createRenderer window (-1) rdrConfig
    let col = SDL.rendererDrawColor renderer
    col SDL.$= black
    SDL.showWindow window
    Game.runGame_ Sample.title renderer
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.Font.quit
    SDL.quit