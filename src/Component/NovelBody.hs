{-# LANGUAGE OverloadedStrings #-}

module Component.NovelBody where

import Game
import qualified SDL
import qualified SDL.Input.Mouse

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

goNext :: NovelGame ()
goNext = do
    st <- get
    put $ st { isIdling = False }

spPageNext :: EventHandler
spPageNext = pressEvent SDL.KeycodeSpace goNext

clickPageNext :: EventHandler
clickPageNext = clickEvent SDL.Input.Mouse.ButtonLeft (0, 400, 800, 200) goNext

simple :: [Component]
simple = [ (unitComponent "bgimg") { eventHandler = [spPageNext] }
         , (unitComponent "dialog")
            { src      = Just "test/img/wafu2.png"
            , font     = Just "font/mplus-1p-regular.ttf"
            , color    = Just (255, 0, 0, 0)
            , position = Just (0, 400)
            , size     = Just (800, 200)
            , padding  = Just (40, 40)
            , isVisible = True
            , eventHandler = [clickPageNext] }
         , (unitComponent "option1")
            { cid = "option1"
            , src = Just "test/img/option.png"
            , font = Just "font/mplus-1p-regular.ttf"
            , color = Just (255, 255, 255, 0)
            , position = Just (290, 150)
            , size = Just (220, 62)
            , padding = Just (50, 15) }]