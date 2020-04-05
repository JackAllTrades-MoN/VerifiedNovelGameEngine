{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game where

import qualified SDL
import Data.Text (Text)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

import Data.Monoid ((<>))

data NovelGameSt = NovelGameSt
      { renderer :: SDL.Renderer
      , font     :: Text
      , dialog   :: Text
      , bgImg    :: FilePath }
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
                 , bgImg    = "test/img/bg.png" }
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

idle :: NovelGame ()
idle = do
    st <- get
    evs <- SDL.pollEvents
    mapM_ handleEvent evs
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
    updateMsg "これは2行目です"