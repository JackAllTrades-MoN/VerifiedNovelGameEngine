{-# LANGUAGE OverloadedStrings #-}

module ScriptParser where

import Text.Parsec.Prim
import Text.Parsec.Token
import Text.Parsec.Combinator
import Text.Parsec.Char
import Data.Text (Text, pack, unpack)
import Text.Parsec.Text (Parser)


data Stmt = UpdateDialog Text
          | Comment Text 
    deriving Show

-- rawStr :: Parser String
-- rawStr = many1 anyChar
-- rawStr = manyTill anyChar (try $ string "\n\n")

updateDialog :: Parser Stmt
updateDialog = UpdateDialog . pack <$>
                 manyTill anyChar (try $ string "\n\n")

comment :: Parser Stmt
comment = Comment . pack <$> do{ string "(*";
              manyTill anyChar (try $ string "*)")}

lineStmt :: Parser Stmt
lineStmt = comment <|> updateDialog

script :: Parser [Stmt]
script = (many lineStmt) <* eof

test = do
    print $ parse script "" "hoge\n\n fool\n\n"
    print $ parse script "" "foo"