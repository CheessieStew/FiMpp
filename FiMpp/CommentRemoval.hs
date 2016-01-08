module CommentRemoval
(removeComments) where

import General
import Text.ParserCombinators.Parsec



removeComments :: GenParser Char st String
removeComments = do res <- manyTill (manyTill (noneOf ")") (try comment <|> eof)) ((ignore $ lookAhead $ char 'l') <|> eof)
                    (notFollowedBy $ char ')') -- eof -> successful removing, ')' -> something's wrong
                    let res2 = concat res
                    return res2

comment :: GenParser Char st ()
comment =  inlineComment <|> blockComment

inlineComment = do string "P." --at least one "P."
                   manyTill (string "P.") (string "S.") -- then any number of "P."s and one "S."
                   manyTill anyChar (ignore eol <|> lookAhead eof) -- till the end of the line or the file everything is a comment
                   return ()

blockComment = do char '('
                  manyTill (manyTill (try $ noneOf ")") (try comment <|> try eof <|> (ignore $ try $ lookAhead $ char ')'))) ((ignore $ char ')') <|> lookAhead eof)
                  -- we make sure that one blockComment eats only ONE ')'
                  return ()
