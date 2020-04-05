module AMachine.Event where

import qualified Data.Text as T

data Event = EKey T.Text | EClose deriving Show