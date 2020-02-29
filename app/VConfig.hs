module VConfig where

data Options = Options
  { optCommand :: Command }

data Command
  = Build
  | Run
  | Debug
