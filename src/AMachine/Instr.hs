module AMachine.Instr where

import Data.Text (Text)
import GHC.Word (Word8)

data Instr =
    IWait
    | IShutdown
    | IAddPicture { targetId :: Text
                  , domid    :: Text
                  , name     :: Text
                  , src      :: FilePath
                  , position :: Maybe (Int, Int)
                  , w        :: Maybe Int
                  , h        :: Maybe Int }
    | IAddText { targetId  :: Text
               , domid     :: Text
               , name      :: Text
               , value     :: Text
               , fontSize  :: Maybe Int
               , color     :: Maybe (Word8, Word8, Word8, Word8)
               , position  :: Maybe (Int, Int)} deriving (Show, Read)

addPicture :: Text -> Text -> Text -> FilePath -> Instr
addPicture target domid name src = IAddPicture
 { targetId = target, domid = domid, name = name, src = src
 , position = Nothing, w = Nothing, h = Nothing}

addText :: Text -> Text -> Text -> Text -> Instr
addText target domid name value = IAddText
 { targetId = target, domid = domid, name = name, value = value
 , fontSize = Nothing, color = Nothing, position = Nothing }