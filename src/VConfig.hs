-- {-# LANGUAGE OverloadedStrings #-}

module VConfig where

import Data.Text (Text)
import Options.Applicative

{-
data Options = Options
  { optCommand :: Command }
-}

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

data Command
  = Build
  | Sample
  | New Text

parseBuild :: Parser Command
parseBuild = pure Build

parseSample :: Parser Command
parseSample = pure Sample

parseNew :: Parser Command
parseNew = New <$> argument str (metavar "[ProjectName]")

parser :: Parser Command
parser = subparser
          ( command "build" (parseBuild `withInfo` "To make a product build")
            <> command "sample" (parseSample `withInfo` "To run a sample game")
            <> command "new" (parseNew `withInfo` "To make a new project"))

parseInfo = parser `withInfo` "VeNGE a Verified Novel Game Engine"
