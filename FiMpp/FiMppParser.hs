
module FiMppParser
(parseFiMppFile) where

import CommentRemoval
import GeneralP
import Text.ParserCombinators.Parsec
import Types
import System.IO

fromRight :: Either a b-> b
fromRight (Right a) = a
fromRight (Left a) = undefined

parseFiMppFile path = do handle <- openFile path ReadMode
                         contents <- hGetContents handle
                         let uncommented = fromRight (removeComments path contents)
                         return $ fiMppFile path uncommented

fiMppFile path contents = parse classDeclaration path contents


classDeclaration = do string "Dear" <?> "proper greetings"
                      spaces
                      (superClass:interfaces) <- classNames
                      many1 space
                      className <- manyTill (letter <|> space) punctuation
                      eol
                      methods
                      string "Your faithful student, "
                      programmerName <-manyTill (letter <|> space) punctuation
                      return (superClass,interfaces)

classNames :: GenParser Char st [String] --TODO: names should be in Title Case
classNames = manyTill className (char ':')
className = do many space
               manyTill (letter <|> space) (more <|> end)
  where more = ignore (try $ many1 space >> string "and")
        end = ignore (lookAhead $ char ':')


methods = undefined
