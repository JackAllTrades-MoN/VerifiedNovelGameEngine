{-# LANGUAGE OverloadedStrings #-}

module AMachine.DOM where

import Data.Text
import GHC.Word (Word8)

data V4 = V4 Word8 Word8 Word8 Word8
data V2 = V2 Int Int

data Element = ERoot
              | EText { value :: Text, fontSize :: Int, color :: V4, position :: V2 }
              | EPicture { src :: FilePath, position :: V2, w :: Maybe Int, h :: Maybe Int }

data DOMTree = DOMTree
    { domid    :: Text
    , name     :: Text
    , element  :: Element
    , children :: [DOMTree] }

init :: DOMTree
init = DOMTree
 { domid    = "root"
 , name     = "root"
 , element  = ERoot
 , children = [] }

addChildren :: DOMTree -> [DOMTree] -> DOMTree
addChildren parent children' = parent { children = children parent ++ children' }

testTree :: DOMTree
testTree =
    let e = EText { value    = "これは日本語のサンプルです"
                  , fontSize = 20
                  , color    = V4 255 100 0 0
                  , position = V2 40 440 }
        ew = EText { value    = "world"
                   , fontSize = 30
                   , color    = V4 0 255 255 0
                   , position = V2 10 20 }
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
                                 , position = V2 0 0
                                 , w = Just 800
                                 , h = Just 600 }
                     , children = []}
        dialog = DOMTree { domid   = "dialog"
                         , name    = "dialog"
                         , element = EPicture
                                 { src = "test/img/wafu2.png"
                                 , position = V2 0 400
                                 , w = Just 800
                                 , h = Just 200 }
                     , children = []}
    in addChildren AMachine.DOM.init [bg, dialog, hello, world]