{-# LANGUAGE OverloadedStrings #-}

module Component.NovelTitle where
import Game
import qualified SDL
import qualified SDL.Input.Mouse

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

simple :: [Component]
simple = [ unitComponent "title_bg"
         , (unitComponent "title_logo")
            { position = Just (0, 400)
            , size = Just (800, 200)}
         , (unitComponent "option1")
            { src = Just "test/img/option.png"
            , txt = Nothing
            , font = Just "font/mplus-1p-regular.ttf"
            , color = Just (255, 255, 255, 0)
            , position = Just (290, 400)
            , size = Just (220, 62)
            , padding = Just (50, 15)
            , isVisible = False }
         , (unitComponent "option2")
            { src = Just "test/img/option.png"
            , txt = Nothing
            , font = Just "font/mplus-1p-regular.ttf"
            , color = Just (255, 255, 255, 0)
            , position = Just (290, 450)
            , size = Just (220, 62)
            , padding = Just (50, 15)
            , isVisible = False }]