{-# LANGUAGE OverloadedStrings #-}

module AMachine.DOM where

import Data.Text
import GHC.Word (Word8)
import qualified Data.List as List

-- data V4 = V4 Word8 Word8 Word8 Word8
-- data V2 = V2 Int Int

data Element = ERoot
              | EText { value :: Text, fontSize :: Int, color :: (Word8, Word8, Word8, Word8), position :: (Int, Int) }
              | EPicture { src :: FilePath, position :: (Int, Int), w :: Maybe Int, h :: Maybe Int }
 deriving Show

data DOMTree = DOMTree
    { domid    :: Text
    , name     :: Text
    , element  :: Element
    , children :: [DOMTree] }
 deriving Show

init :: DOMTree
init = DOMTree
 { domid    = "root"
 , name     = "root"
 , element  = ERoot
 , children = [] }

addChildren :: DOMTree -> [DOMTree] -> DOMTree
addChildren parent children' = parent { children = children parent ++ children' }

addChildTo :: Text -> DOMTree -> DOMTree -> DOMTree
addChildTo targetId child cur
    | domid cur == targetId = cur { children = children cur ++ [child]}
    | otherwise = cur { children =
                         List.map (addChildTo targetId child) $ children cur }

{-
testTree :: DOMTree
testTree =
    let e = EText { value    = "これは日本語のサンプルです"
                  , fontSize = 20
                  , color    = (255, 100, 0, 0)
                  , position = (40, 440) }
        ew = EText { value    = "world"
                   , fontSize = 30
                   , color    = (0, 255, 255, 0)
                   , position = (10, 20) }
        hello = DOMTree { domid    = "hello"
                        , name     = "hello"
                        , element  = e
                        , children = [] }
        world = DOMTree { domid    = "world"
                        , name     = "world"
                        , element  = ew
                        , children = [] }
        bg = DOMTree { domid   = "bg"
                     , name    = "bg"
                     , element = EPicture
                                 { src = "test/img/bg.png"
                                 , position = (0, 0)
                                 , w = Just 800
                                 , h = Just 600 }
                     , children = []}
        dialog = DOMTree { domid   = "dialog"
                         , name    = "dialog"
                         , element = EPicture
                                 { src = "test/img/wafu2.png"
                                 , position = (0, 400)
                                 , w = Just 800
                                 , h = Just 200 }
                     , children = []}
    in addChildren AMachine.DOM.init [bg, dialog, hello, world] -}