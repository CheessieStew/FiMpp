
module FiMppParser
() where

import CommentRemoval
import General
import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Types
import System.IO



parseFile parser path = do handle <- openFile path ReadMode
                           contents <- hGetContents handle
                           let result = parse removeComments path contents
                           let uncommented = fromRight result
                           if (isLeft result) then fail uncommented else return ()
                           let parsed = parse parser path uncommented
                           return parsed
  where fromRight (Right b) = b
        fromRight (Left a) = fail (concatMap messageString $ errorMessages a)
        isLeft (Right b) = False
        isLeft (Left a) = True

fiMppFile path contents = parse (method []) path contents


classDeclaration = do string "Dear" <?> "proper greetings"
                      spaces
                      (superClass:interfaces) <- classNames
                      many1 space
                      className <- manyTill (letter <|> space) punctuation
                      eol
                      methods (superClass:interfaces)
                      string "Your faithful student, "
                      programmerName <-manyTill (letter <|> space) punctuation
                      return (superClass,interfaces)
  where classNames = manyTill className (char ':')
        className = do many space
                       optional (string "and" >> many space) <?> "another name"
                       notFollowedBy (char ':') <?> "another name"
                       manyTill (letter <|> space) (more <|> end)
        more = ignore (try $ many1 space >> (lookAhead $ string "and"))
        end = ignore (try $ many space >> (lookAhead $ char ':'))

methods reserved= undefined

method reserved = do mainMethod <- checker (string "Today")
                     many space
                     string "I learned"
                     many1 space
                     name <- methodName
                     if (elem name reserved) then fail "Illegal method name" else return ()
                     methodReturns <- (returns >> many space >> someType) <|> return None
                     methodArguments <- (args >> many space >> arguments) <|> return []
                     many space
                     punctuation
                     many space
                     --TODO: instructions!
                     string "That's all about"
                     many1 space
                     string name
                     many space
                     punctuation
                     return Method {isMain = mainMethod,
                                    name = name,
                                    returned = methodReturns,
                                    arguments = methodArguments,
                                    root = Placeholder}
  where methodName = manyTill (letter <|> space) ((lookAhead returns) <|> (lookAhead args) <|> (lookAhead $ try $ many space >> try punctuation) <?> "punctuation")
        returns = (try $ many1 space >> (string "with" <|> string "to get" <|> string "as"))
        args = (try $ many1 space >> string "using")
        arguments = manyTill argument (try $ lookAhead punctuation)
        argument = do many space
                      t <- someType
                      many space
                      name <- manyTill (letter <|> space) (try space >> ((lookAhead $ try punctuation) <|> (lookAhead $ try $ string "and")))
                      optional (string "and")
                      return (name,t)

someType = do first <- ((try $ string "an") <|> (try $ string "a") <|> (try $ string "the") <|> return "")
              many space
              if (first == "an" || first == "a") then notFollowedBy (string "many") else return ()
              many space
              array1 <- ((try $ string "many") <|> return "") --only after a "the" or nothing
              many space
              res <- choice (map (\s -> (try $ string s) <?> "type") legalTypes)
              array2 <- checker (string "es" <|> string "s")
              if (array1 == "many") && (array2 == False) then fail "\"many\" used with singular" else return ()
              let res2 = actualType res
              if res2 == None then fail "Illegal type" else return ()
              if array2 then return (Many res2) else return res2
variableName = undefined
