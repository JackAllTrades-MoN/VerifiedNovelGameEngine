{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game where

import qualified SDL
import qualified SDL.Font
import qualified SDL.Video.Renderer
import Data.Text (Text)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

import Unsafe.Coerce (unsafeCoerce)
import Data.Monoid ((<>))

data NovelGameSt = NovelGameSt
      { renderer :: SDL.Renderer
      , font     :: FilePath
      , dialog   :: Text
      , bgImg    :: FilePath
      , isScreenUpdated :: Bool }
    deriving Show

newtype NovelGame a = NovelGame (StateT NovelGameSt (ExceptT String IO) a)
  deriving (Functor, Applicative, Monad, MonadState NovelGameSt, MonadError String, MonadIO)

runGame :: NovelGame () -> NovelGameSt -> IO (Either String ((), NovelGameSt))
runGame (NovelGame ng) initSt = runExceptT (runStateT ng initSt)

runGame_ :: NovelGame () -> SDL.Renderer -> IO ()
runGame_ ng renderer  = do
    let initSt = NovelGameSt
                 { renderer = renderer
                 , font     = "font/mplus-1p-regular.ttf"
                 , dialog   = ""
                 , bgImg    = "test/img/bg.png"
                 , isScreenUpdated = False }
    result <- runGame ng initSt
    print result

handleEvent :: SDL.Event -> NovelGame ()
handleEvent ev = do
    st <- get
    aux $ SDL.eventPayload ev
    where
        aux :: SDL.EventPayload -> NovelGame ()
        aux (SDL.KeyboardEvent kev)
         | SDL.keyboardEventKeyMotion kev == SDL.Pressed
          = liftIO $ putStrLn $ "Press"
        aux (SDL.WindowClosedEvent wev) = throwError "Exit: code 0"
        aux _ = return ()

printMsg :: Int -> Int -> Int -> Text -> NovelGame ()
printMsg x y size msg = do
  let color = SDL.V4 255 0 0 0 :: SDL.Font.Color
  st <- get
  font <- liftIO $ SDL.Font.load (font st) size
  surface <- liftIO $ SDL.Font.solid font color msg
  dim <- liftIO $ SDL.Video.Renderer.surfaceDimensions surface
  txtr <- liftIO $ SDL.createTextureFromSurface (renderer st) surface
  let rect = SDL.Rectangle (SDL.P $ SDL.V2 (unsafeCoerce x) (unsafeCoerce y)) dim
  liftIO $ SDL.copy (renderer st) txtr Nothing (Just rect)

printScreen :: NovelGame ()
printScreen = do
    st <- get
    aux st
    where
        aux st
         | not $ isScreenUpdated st = do
            liftIO $ SDL.clear (renderer st)
            st <- get
            printMsg 0 0 20 "hoge"
            liftIO $ SDL.present (renderer st)
            put $ st { isScreenUpdated = True }
         | otherwise = return ()

idle :: NovelGame ()
idle = do
    evs <- SDL.pollEvents
    mapM_ handleEvent evs
    printScreen
    idle

updateMsg :: Text -> NovelGame ()
updateMsg msg = do
    st <- get
    put $ st { dialog = msg }

loadBGImage :: FilePath -> NovelGame ()
loadBGImage path = do
    st <- get
    put $ st { bgImg = path }

test :: NovelGame ()
test = do
    loadBGImage "test/img/bg.png"
    updateMsg "これは1行目です"
    idle
    updateMsg "これは2行目です"