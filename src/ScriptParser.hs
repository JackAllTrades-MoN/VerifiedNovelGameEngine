{-# LANGUAGE OverloadedStrings #-}

module ScriptParser where

import Text.Parsec.Prim
import Text.Parsec.Token
import Text.Parsec.Combinator
import Text.Parsec.Char
import Data.Text (Text, pack, unpack, replace)
import Text.Parsec.Text (Parser)
import Text.Parsec.Error
import qualified Data.Text.IO as TIO
import qualified Text.Parsec.Pos as TP
import qualified Data.List as List

data Stmt = UpdateDialog Text
          | UpdateSentence Text Text
          | Command Text  
          | Comment Text 
    deriving Show

data Tk = BR           -- \n
        | BRBR         -- \n\n
        | RAWSTR Text  -- text
        | DQ           -- "
        | LSB          -- [
        | RSB          -- ]
        | LStar        -- (*
        | RStar        -- *)
        | COLON        -- :
    deriving (Show, Eq)

type PTk = (Tk, TP.SourcePos)

tkSymbol :: Text -> Tk -> Parser PTk
tkSymbol str tk = do
    pos <- getPosition
    string $ unpack str
    return (tk, pos)

tkbr :: Parser PTk
tkbr = tkSymbol "\n" BR
tkbrbr :: Parser PTk
tkbrbr = tkSymbol "\n\n" BRBR
tkrawstr :: Parser PTk
tkrawstr = do
    pos <- getPosition
    (\str -> (RAWSTR $ pack str, pos))
            <$> many1 (noneOf $ unpack "\n[](**):")
tkdq :: Parser PTk
tkdq = tkSymbol "\"" DQ
tklsb :: Parser PTk
tklsb = tkSymbol "[" LSB
tkrsb :: Parser PTk
tkrsb = tkSymbol "]" RSB
tklstar :: Parser PTk
tklstar = tkSymbol "(*" LStar
tkrstar :: Parser PTk
tkrstar = tkSymbol "*)" RStar
tkcolon :: Parser PTk
tkcolon = tkSymbol ":" COLON

tk :: Parser PTk
tk = try tkbrbr <|> tkbr
     <|> tkdq <|> tklsb <|> tkrsb
     <|> tklstar <|> tkrstar <|> tkcolon <|> tkrawstr

script :: Parser [PTk]
script = many tk

lexer :: Text -> Either Text.Parsec.Error.ParseError [PTk]
lexer = parse script "testscr.txt"

tksym :: Tk -> Parsec [PTk] () ()
tksym tk = token (show . fst) snd ptkParse
    where
        ptkParse (tk', _) | tk' == tk = Just () | otherwise = Nothing

rawText :: Parsec [PTk] () Text
rawText = token (show . fst) snd ptkParse
    where
        ptkParse (RAWSTR txt, _) = Just txt
        ptkParse _ = Nothing

updateDialog :: Parsec [PTk] () Stmt
updateDialog = UpdateDialog <$> rawText

updateSentence :: Parsec [PTk] () Stmt
updateSentence =
    (UpdateSentence <$> (rawText <* (tksym COLON))) <*> rawText

tokenToText :: PTk -> Text
tokenToText (BR, _) = "\n"
tokenToText (BRBR, _) = "\n\n"
tokenToText (RAWSTR txt, _) = txt
tokenToText (DQ, _) = "\""
tokenToText (LSB, _) = "["
tokenToText (RSB, _) = "]"
tokenToText (LStar, _) = "(*"
tokenToText (RStar, _) = "*)"
tokenToText (COLON, _) = ":"

comment :: Parsec [PTk] () Stmt
comment = Comment . (List.foldl (\acc a -> acc <> (tokenToText a)) "")
            <$> (tksym LStar
                        *> (manyTill anyToken (try $ tksym RStar)))

lineStmt :: Parsec [PTk] () Stmt
lineStmt = try comment <|> try updateSentence <|> updateDialog

stmtParse :: Text -> Either Text.Parsec.Error.ParseError [Stmt]
stmtParse txt = do
    tks <- lexer txt
    parse stmt "hoge.txt" tks
        where stmt = lineStmt `sepBy` (tksym BRBR)

test :: IO ()
test = do
    testScript <- TIO.readFile "test/testscr.txt"
    print $ lexer testScript
    print $ stmtParse testScript