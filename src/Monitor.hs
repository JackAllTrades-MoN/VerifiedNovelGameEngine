{-# LANGUAGE OverloadedStrings #-}

module Monitor where

import SDL
import SDL.Font
import Data.Text
import Data.Maybe (fromMaybe)
import SDL.Video.Renderer (surfaceDimensions)
import Foreign.C.Types (CInt, CInt(..))
import Unsafe.Coerce (unsafeCoerce)
import SDL.Image

import AMachine
import AMachine.DOM as DOM

red :: SDL.Font.Color
red = SDL.V4 255 0 0 0

toSDLColor :: DOM.V4 -> SDL.Font.Color
toSDLColor (DOM.V4 r g b a) = SDL.V4 r g b a

defaultFont = "font/mplus-1p-regular.ttf"

printMsg :: SDL.Renderer -> SDL.Font.Color -> Int -> Int -> Int -> Text -> IO ()
printMsg renderer color x y size msg = do
  font <- SDL.Font.load defaultFont size
  surface <- SDL.Font.solid font color msg
  dim <- SDL.Video.Renderer.surfaceDimensions surface
  txtr <- SDL.createTextureFromSurface renderer surface
  let rect = SDL.Rectangle (P $ SDL.V2 (unsafeCoerce x) (unsafeCoerce y)) dim
  SDL.copy renderer txtr Nothing (Just rect)

printPicture :: SDL.Renderer -> Int -> Int -> Maybe Int -> Maybe Int -> FilePath -> IO ()
printPicture renderer x y w h filePath = do
  surface <- SDL.Image.load filePath
  SDL.V2 w' h' <- SDL.Video.Renderer.surfaceDimensions surface
  txtr <- SDL.createTextureFromSurface renderer surface
  let dim  = SDL.V2 (fromMaybe w' (fmap unsafeCoerce w)) (fromMaybe h' (fmap unsafeCoerce h))
      rect = SDL.Rectangle (P $ SDL.V2 (unsafeCoerce x) (unsafeCoerce y)) dim
  SDL.copy renderer txtr Nothing (Just rect)

printDOMTree :: SDL.Renderer -> DOM.DOMTree -> IO ()
printDOMTree renderer dom @ DOM.DOMTree { element = EText {} } = do
  let e = element dom
      DOM.V2 x y = position e
  printMsg renderer (toSDLColor $ color e) x y (fontSize e) (value e)
  mapM_ (printDOMTree renderer) $ children dom

printDOMTree renderer dom @ DOM.DOMTree { element = EPicture {} } = do
  let e = element dom
      DOM.V2 x y = position e
  printPicture renderer x y (w e) (h e) $ src e

printDOMTree renderer dom =
  mapM_ (printDOMTree renderer) $ children dom

printScreen :: SDL.Renderer -> AMachine.MachineState -> IO ()
printScreen renderer MachineState { domTree = dt, isDOMUpdated = True } = do
    SDL.clear renderer
    printDOMTree renderer dt
    SDL.present renderer

printScreen _ _ = return ()