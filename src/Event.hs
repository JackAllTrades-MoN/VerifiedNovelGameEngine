-- {-# LANGUAGE OverloadedStrings #-}

module Event where

{-
data InputMotion = Released | Pressed
data MouseButton = ButtonLeft | ButtonMiddle | ButtonRight

data Event =
    KeyboardEvent { keyboardEventKeyMotion :: InputMotion,
                    keyboardEventKeysym :: Keysym }
    | MouseMotionEvent { mouseMotionEventState :: [MouseButton],
                         mouseMotionEventPos :: (Int, Int) }
    | MouseButtonEvent { SDL.mouseButtonEventMotion :: InputMotion,
                         SDL.mouseButtonEventButton :: MouseButton,
                              SDL.mouseButtonEventClicks :: GHC.Word.Word8,
                              SDL.mouseButtonEventPos :: !(SDL.Point SDL.V2 GHC.Int.Int32)}
    | MouseWheelEvent
    -}
{-
data Event = 
  | SDL.KeyboardEvent !SDL.KeyboardEventData
  | SDL.MouseMotionEvent !SDL.MouseMotionEventData
  | SDL.MouseButtonEvent !SDL.MouseButtonEventData
  | SDL.MouseWheelEvent !SDL.MouseWheelEventData
  -}