{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase #-}

module Game where

import qualified SDL
import qualified SDL.Font
import qualified SDL.Image
import qualified SDL.Video.Renderer
import qualified SDL.Input.Mouse
import GHC.Word (Word8)
import Data.Text (Text)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import Unsafe.Coerce (unsafeCoerce)
import Data.Monoid ((<>))

newtype EventHandler = EventHandler (SDL.EventPayload -> NovelGame ())
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
                    , isVisible :: Bool
                    , eventHandler :: [EventHandler] }
    deriving Show

unitComponent :: Text -> Component
unitComponent cid = Component {
    cid = cid, src = Nothing, txt = Nothing, font = Nothing,
    color = Nothing, position = Nothing, size = Nothing,
    padding = Nothing, depth = Nothing, isVisible = True,
    eventHandler = [] }

data NovelGameSt = NovelGameSt
      { renderer        :: SDL.Renderer
      , isScreenUpdated :: Bool
      , isIdling   :: Bool
      , components :: [Component] }
    deriving Show

newtype NovelGame a = NovelGame (StateT NovelGameSt (ExceptT Text IO) a)
  deriving (Functor, Applicative, Monad, MonadState NovelGameSt, MonadError Text, MonadIO)

runGame :: NovelGame () -> NovelGameSt -> IO (Either Text ((), NovelGameSt))
runGame (NovelGame ng) initSt = runExceptT (runStateT ng initSt)

runGame_ :: NovelGame () -> SDL.Renderer -> IO ()
runGame_ ng renderer  = do
    let initSt = NovelGameSt
                 { renderer        = renderer
                 , isScreenUpdated = False
                 , isIdling        = False
                 , components      = [] }
    result <- runGame ng initSt
    print result

handleEvent :: SDL.Event -> NovelGame ()
handleEvent ev = do
    st <- get
    let evp = SDL.eventPayload ev
    aux evp
    mapM_ ((mapM_ (\(EventHandler eh) -> eh evp)) . eventHandler) $ components st
    where
        aux :: SDL.EventPayload -> NovelGame ()
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

updateComponent :: Text -> (Component -> Component) -> NovelGame ()
updateComponent cid' updater = do
    st <- get
    put $ st { isScreenUpdated = False
             , components = List.map updater (components st)}

updateTxt :: Text -> Text -> NovelGame ()
updateTxt cid' msg =
    updateComponent cid' updater
    where
        updater c
         | cid c == cid' = c { txt = Just msg }
         | otherwise = c

updateSrc :: Text -> FilePath -> NovelGame ()
updateSrc cid' src' =
    updateComponent cid' updater
    where
        updater c
         | cid c == cid' = c { src = Just src' }
         | otherwise = c

updateVisibility :: Text -> Bool -> NovelGame ()
updateVisibility cid' isVisible =
    updateComponent cid' updater
    where
        updater c
            | cid c == cid' = c { isVisible = isVisible }
            | otherwise = c 

getDim :: Text -> NovelGame (Int, Int, Int, Int)
getDim cid' = do
    st <- get
    case List.find (\c -> cid c == cid') $ components st of
         Nothing -> throwError $ "Unknown ID: " <> cid'
         Just c ->
          let (x, y) = Maybe.fromMaybe (0, 0) $ position c
              (w, h) = Maybe.fromMaybe (800, 600) $ size c
          in return (x, y, w, h)

addEvent :: Text -> EventHandler -> NovelGame ()
addEvent cid' ev =
    updateComponent cid' updater
    where
        updater c
            | cid c == cid' = c { eventHandler = ev : eventHandler c }
            | otherwise = c

enableOption :: Text -> Text -> NovelGame () -> NovelGame ()
enableOption cid' msg action = do
    (x, y, w, h) <- getDim cid'
    addEvent cid' $
     clickEvent SDL.Input.Mouse.ButtonLeft (x, y, w, h) action'
    updateTxt cid' msg
    updateVisibility cid' True
    where action' = do
                    clearEvent cid'
                    updateVisibility cid' False
                    action

clearEvent :: Text -> NovelGame ()
clearEvent cid' =
    updateComponent cid' updater
    where
        updater c
         | cid c == cid' = c { eventHandler = [] }
         | otherwise = c

clickEvent :: SDL.Input.Mouse.MouseButton -> (Int, Int, Int, Int) -> NovelGame () -> EventHandler
clickEvent buttonType range action =
    EventHandler aux
    where
        aux (SDL.MouseButtonEvent mbev)
            | SDL.mouseButtonEventButton mbev == buttonType &&
              inRange range (SDL.mouseButtonEventPos mbev) = action
            | otherwise = return ()
        aux _ = return ()
        inRange (x, y, w, h) (SDL.P (SDL.V2 px py))
         = let (x', y', w', h') = ( unsafeCoerce x
                                  , unsafeCoerce y
                                  , unsafeCoerce w
                                  , unsafeCoerce h)
           in x' < px && y' < py && px < x' + w' && py < y' + h'

pressEvent :: SDL.Keycode -> NovelGame () -> EventHandler
pressEvent keycode action =
    EventHandler aux
    where 
        aux (SDL.KeyboardEvent kev)
            | SDL.keyboardEventKeyMotion kev == SDL.Pressed &&
             SDL.keysymKeycode (SDL.keyboardEventKeysym kev) == keycode = action
        aux _ = return ()

loadComponents :: [Component] -> NovelGame ()
loadComponents comps = do
    st <- get
    put $ st { components = comps }

testComponent :: NovelGame ()
testComponent = do
    let a = get >>= \st ->
            put (st { isIdling = False }) >>= \() ->
            liftIO $ putStrLn "Press Space"
    let ev = pressEvent SDL.KeycodeSpace a
    let components = [Component { cid  = "bg"
                                , src = Just "test/img/bg.png"
                                , txt = Nothing
                                , font = Nothing
                                , color = Nothing
                                , position = Nothing
                                , size = Nothing
                                , padding = Nothing
                                , depth = Nothing
                                , isVisible = True
                                , eventHandler = [ev] },
                      Component { cid  = "dialog"
                                , src = Just "test/img/wafu2.png"
                                , txt = Nothing
                                , font = Nothing
                                , color = Nothing
                                , position = Just (0, 400)
                                , size = Just (800, 200)
                                , padding = Nothing
                                , depth = Nothing
                                , isVisible = True
                                , eventHandler = [] },
                      Component { cid  = "textarea"
                                , src = Nothing
                                , txt = Just "Dummy Text"
                                , font = Just "font/mplus-1p-regular.ttf"
                                , color = Just (255, 0, 0, 0)
                                , position = Just (40, 440)
                                , size = Nothing
                                , padding = Nothing
                                , depth = Nothing
                                , isVisible = True
                                , eventHandler = [] },
                      Component { cid = "option1"
                                , src = Just "test/img/option.png"
                                , txt = Just "option1"
                                , font = Just "font/mplus-1p-regular.ttf"
                                , color = Just (255, 255, 255, 0)
                                , position = Just (290, 150)
                                , size = Just (220, 62)
                                , padding = Just (50, 15)
                                , depth = Nothing
                                , isVisible = False
                                , eventHandler = []}]
    st <- get
    put st { components = components }
