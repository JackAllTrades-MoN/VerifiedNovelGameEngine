{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game where

import qualified SDL
import qualified SDL.Font
import qualified SDL.Image
import qualified SDL.Video.Renderer
import GHC.Word (Word8)
import Data.Text (Text)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Unsafe.Coerce (unsafeCoerce)
import Data.Monoid ((<>))

newtype EventHandler = EventHandler (SDL.Event -> NovelGame ())
instance Show EventHandler where
    show (EventHandler eh) = "<EventHandler>"

data Component = Component
                    { cid        :: Text
                    , src       :: Maybe FilePath
                    , txt       :: Maybe Text
                    , font      :: Maybe FilePath
                    , color     :: Maybe (Word8, Word8, Word8, Word8)
                    , position  :: Maybe (Int, Int)
                    , size      :: Maybe (Int, Int)
                    , padding   :: Maybe (Int, Int)
                    , depth     :: Maybe Int
                    , isVisible :: Bool }
    deriving Show

data NovelGameSt = NovelGameSt
      { renderer        :: SDL.Renderer
      , isScreenUpdated :: Bool
      , isIdling   :: Bool
      , components :: [Component] 
      , evListener :: [EventHandler] }
    deriving Show

newtype NovelGame a = NovelGame (StateT NovelGameSt (ExceptT String IO) a)
  deriving (Functor, Applicative, Monad, MonadState NovelGameSt, MonadError String, MonadIO)

runGame :: NovelGame () -> NovelGameSt -> IO (Either String ((), NovelGameSt))
runGame (NovelGame ng) initSt = runExceptT (runStateT ng initSt)

runGame_ :: NovelGame () -> SDL.Renderer -> IO ()
runGame_ ng renderer  = do
    let initSt = NovelGameSt
                 { renderer        = renderer
                 , isScreenUpdated = False
                 , isIdling        = False
                 , components      = []
                 , evListener      = [] }
    result <- runGame ng initSt
    print result

handleEvent :: SDL.Event -> NovelGame ()
handleEvent ev = do
    st <- get
    aux $ SDL.eventPayload ev
    where
        aux :: SDL.EventPayload -> NovelGame ()
        aux (SDL.KeyboardEvent kev)
         | SDL.keyboardEventKeyMotion kev == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym kev) == SDL.KeycodeQ
                = do
                    st <- get
                    liftIO $ putStrLn "Press Q"
                    put $ st { isIdling = False }
         | SDL.keyboardEventKeyMotion kev == SDL.Pressed
          = liftIO $ putStrLn $ "Press"
        aux (SDL.WindowClosedEvent wev) = throwError "Exit: code 0"
        aux _ = return ()

printMsg :: SDL.Font.Color -> FilePath -> Int -> Int -> Int -> Text -> NovelGame ()
printMsg color fontPath x y size msg = do
  st <- get
  font <- liftIO $ SDL.Font.load fontPath size
  surface <- liftIO $ SDL.Font.solid font color msg
  dim <- liftIO $ SDL.Video.Renderer.surfaceDimensions surface
  txtr <- liftIO $ SDL.createTextureFromSurface (renderer st) surface
  let rect = SDL.Rectangle (SDL.P $ SDL.V2 (unsafeCoerce x) (unsafeCoerce y)) dim
  liftIO $ SDL.copy (renderer st) txtr Nothing (Just rect)

printPicture :: Int -> Int -> Int -> Int -> FilePath -> NovelGame ()
printPicture x y w h filePath = do
    st <- get
    surface <- liftIO $ SDL.Image.load filePath
--    SDL.V2 w' h' <- liftIO $ SDL.Video.Renderer.surfaceDimensions surface
    txtr <- liftIO $ SDL.createTextureFromSurface (renderer st) surface
    let dim  = SDL.V2 (unsafeCoerce w) (unsafeCoerce h)
        rect = SDL.Rectangle (SDL.P $ SDL.V2 (unsafeCoerce x) (unsafeCoerce y)) dim
    liftIO $ SDL.copy (renderer st) txtr Nothing (Just rect)

printComponent :: Component -> NovelGame ()
printComponent c
    | isVisible c = do
    let (x, y) = Maybe.fromMaybe (0, 0) $ position c
        (w, h) = Maybe.fromMaybe (800, 600) $ size c
        (r,g,b,a)  = Maybe.fromMaybe (0, 0, 0, 0) $ color c
        (pl, pt)   = Maybe.fromMaybe (0, 0) $ padding c
        fontPath = Maybe.fromMaybe "font/mplus-1p-regular.ttf" $ font c
    mapM_ (printPicture x y w h) $ src c
    mapM_ (printMsg (SDL.V4 r g b a) fontPath (x + pl) (y + pt) 20) $ txt c
    | otherwise = return ()

printScreen :: NovelGame ()
printScreen = do
    st <- get
    aux st
    where
        aux st
         | not $ isScreenUpdated st = do
            liftIO $ SDL.clear (renderer st)
            st <- get
            mapM_ printComponent $ components st
            liftIO $ SDL.present (renderer st)
            put $ st { isScreenUpdated = True }
         | otherwise = return ()

idle :: NovelGame ()
idle = do
    st <- get
    put $ st { isIdling = True }
    evs <- SDL.pollEvents
    mapM_ handleEvent evs
    printScreen
    st <- get
    when (isIdling st) idle

updateTxt :: Text -> Text -> NovelGame ()
updateTxt cid' msg = do
    st <- get
    put $ st { isScreenUpdated = False
             , components = List.map updater (components st) }
    where
        updater c
         | cid c == cid' = c { txt = Just msg }
         | otherwise = c

updateSrc :: Text -> FilePath -> NovelGame ()
updateSrc cid' src' = do
    st <- get
    put $ st { isScreenUpdated = False
             , components = List.map updater (components st) }
    where
        updater c
         | cid c == cid' = c { src = Just src' }
         | otherwise = c

enableOption :: Text -> Text -> NovelGame ()
enableOption cid' msg = do
    st <- get
    put $ st { isScreenUpdated = False
             , components = List.map updater (components st) }
    where
        updater c
            | cid c == cid' = c { txt = Just msg, isVisible = True }
            | otherwise = c 

testComponent :: NovelGame ()
testComponent = do
    let components = [Component { cid  = "bg"
                                , src = Just "test/img/bg.png"
                                , txt = Nothing
                                , font = Nothing
                                , color = Nothing
                                , position = Nothing
                                , size = Nothing
                                , padding = Nothing
                                , depth = Nothing
                                , isVisible = True },
                      Component { cid  = "dialog"
                                , src = Just "test/img/wafu2.png"
                                , txt = Nothing
                                , font = Nothing
                                , color = Nothing
                                , position = Just (0, 400)
                                , size = Just (800, 200)
                                , padding = Nothing
                                , depth = Nothing
                                , isVisible = True },
                      Component { cid  = "textarea"
                                , src = Nothing
                                , txt = Just "Dummy Text"
                                , font = Just "font/mplus-1p-regular.ttf"
                                , color = Just (255, 0, 0, 0)
                                , position = Just (40, 440)
                                , size = Nothing
                                , padding = Nothing
                                , depth = Nothing
                                , isVisible = True },
                      Component { cid = "option1"
                                , src = Just "test/img/option.png"
                                , txt = Just "option1"
                                , font = Just "font/mplus-1p-regular.ttf"
                                , color = Just (255, 255, 255, 0)
                                , position = Just (290, 150)
                                , size = Just (220, 62)
                                , padding = Just (50, 15)
                                , depth = Nothing
                                , isVisible = False}]
    st <- get
    put st { components = components }

test :: NovelGame ()
test = do
    testComponent
    updateTxt "textarea" "これは1行目です"
    idle
    updateTxt "textarea" "これは2行目です"
    idle
    test2

test2 = do
    updateTxt "textarea" "ここはシーン2です"
    idle
    enableOption "option1" "最初の選択肢"
    idle