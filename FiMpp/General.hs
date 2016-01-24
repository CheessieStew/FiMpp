module General
( eol
, ignore
, punctuation
, checker) where

import Text.ParserCombinators.Parsec

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

punctuation =   try (string "...")
            <|> try (string "!!!")
            <|> try (string "???")
            <|> string "."
            <|> string "!"
            <|> string "?"
            <|> string ":"
            <?> "punctuation"

ignore p = do p
              return ()

checker p = do res <- try p <|> return ""
               if res=="" then return False else return True
