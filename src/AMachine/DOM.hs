{-# LANGUAGE OverloadedStrings #-}

module AMachine.DOM where

import Data.Text
import GHC.Word (Word8)

data V4 = V4 Word8 Word8 Word8 Word8
data V2 = V2 Int Int

data Element = ERoot
              | EText { value :: Text, fontSize :: Int, color :: V4, position :: V2 }
              | EPicture

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
    let e = EText { value    = "hello world"
                  , fontSize     = 20
                  , color    = V4 255 100 0 0
                  , position = V2 10 20 }
        hello = DOMTree { domid    = "hello"
                        , name     = "hello"
                        , element  = e
                        , children = [] }
    in addChildren AMachine.DOM.init [hello]