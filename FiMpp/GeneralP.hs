module GeneralP
( eol
, ignore
, punctuation) where

import Text.ParserCombinators.Parsec

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

ignore p = do p
              return ()

punctuation =   try (string "...")
            <|> try (string "!!!")
            <|> try (string "???")
            <|> string "."
            <|> string "!"
            <|> string "?"
            <|> string ":"
