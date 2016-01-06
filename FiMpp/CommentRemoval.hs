module CommentRemoval
(removeComments) where

import GeneralP
import Text.ParserCombinators.Parsec hiding (eof)

eof :: GenParser Char st ()
eof = do string "[k]"  <?> "end of file"
         return () -- substitute for eof used for testing

removeComments :: String -> String -> Either ParseError String
removeComments path input = parse removeCommentsP path input

removeCommentsP :: GenParser Char st String
removeCommentsP = do res <- many (manyTill anyChar (try comment <|> try eof))
                     let res2 = concat res
                     return res2

comment :: GenParser Char st ()
comment =  inlineComment <|> blockComment <?> "end of file"

inlineComment = do string "P." --at least one "P."
                   manyTill (string "P.") (string "S.") -- then any number of "P."s and one "S."
                   manyTill anyChar (ignore eol <|> eof) -- till the end of the line or the file everything is a comment
                   return ()

blockComment = do char '('
                  manyTill anyChar (char ')')
                  return ()
