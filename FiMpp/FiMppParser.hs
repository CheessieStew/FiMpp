{-#LANGUAGE FlexibleContexts #-}
module FiMppParser(parseFile) where

import CommentRemoval
import General
import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Types
import System.IO
import Data.Maybe


parseFile parser path = do handle <- openFile path ReadMode
                           contents <- hGetContents handle
                           let result = parse removeComments path contents
                           let uncommented = fromRight result
                           if (isLeft result) then fail uncommented else return ()
                           let parsed = parse parser path uncommented
                           return parsed
  where fromRight (Right b) = b
        fromRight (Left a) = fail (concatMap messageString $ errorMessages a)
        isLeft (Right _) = False
        isLeft (Left _) = True

fiMppFile path contents = parse (method []) path contents


classDeclaration = do string "Dear" <?> "proper greetings"
                      spaces
                      (superClass:interfaces) <- classNames
                      many1 space
                      className <- manyTill (letter <|> space) punctuation
                      many space
                      paragraphs <- methods (superClass:interfaces)
                      string "Your faithful student, "
                      programmerName <-manyTill (letter <|> space) punctuation
                      return (Class(superClass,interfaces,className,paragraphs,programmerName))
  where classNames = manyTill className (char ':')
        className = do many space
                       optional (string "and" >> many space) <?> "another name"
                       notFollowedBy (char ':') <?> "another name"
                       manyTill (letter <|> space) (more <|> end)
        more = ignore (try $ many1 space >> (lookAhead $ string "and"))
        end = ignore (try $ many space >> (lookAhead $ char ':'))

methods reserved = manyTill (try $ method reserved) (many space >> (try $ lookAhead $ string "Your faithful student"))

method reserved = do mainMethod <- checker (string "Today")
                     many space
                     string "I learned"
                     many1 space
                     name <- methodName
                     if (elem name reserved) then fail "Illegal method name" else return ()
                     methodReturns <- (returns >> many space >> someType) <|> return NoType
                     methodArguments <- (args >> many space >> arguments) <|> return []
                     -- if mainMethod && (methodReturns /= NoType || methodArguments /= []) then fail "Main method should not have arguments or return type" else return ()
                     -- should replace the following line:
                     if mainMethod && methodArguments /= [] then fail "Main method should not have any arguments" else return ()
                     many space
                     punctuation
                     many space
                     ins <- (instructions reserved methodReturns methodArguments) <|> return [NoInstruction]
                     string "That's all about"
                     many1 space
                     string name
                     many space
                     punctuation
                     return Method {isMain = mainMethod,
                                    name = name,
                                    returned = methodReturns,
                                    arguments = methodArguments,
                                    orders = ins}
  where methodName = manyTill (letter <|> space) ((lookAhead returns) <|> (lookAhead args) <|> (lookAhead $ try $ many space >> try punctuation) <?> "punctuation")
        returns = (try $ many1 space >> (string "with" <|> string "to get" <|> string "as"))
        args = (try $ many1 space >> string "using")
        arguments = manyTill argument (try $ lookAhead punctuation)
        argument = do many space
                      t <- someType
                      many space
                      name <- manyTill (letter <|> space) (try space >> ((lookAhead $ try punctuation) <|> (lookAhead $ try $ string "and")))
                      optional (string "and")
                      return (Variable(name,t))

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
              if res2 == NoType then fail "Illegal type" else return res2
              -- TODO: modify when arrays are implemented

instructions reserved ret vars = do (ins,var) <- try (instruction reserved vars)
                                    let nvars = if isJust var then (fromJust var):vars else vars
                                    many space
                                    rest <- remainingInstructions nvars
                                    return (ins:rest)
  where final vars= do  res <- if (ret/= NoType) then returnInstruction ret vars else return NoInstruction
                        many space
                        lookAhead (string "That's all about")
                        return [res]
        remainingInstructions vars = (final vars) <|> instructions reserved ret vars

instruction reserved vars = varDeclaration <|> try increment <|> try decrement <|> try insMethodCall <?> "instruction"
  where varDeclaration = do string "Did you know that"
                            many1 space
                            varname <- manyTill (letter <|> space) (try (many1 space >> assignKeyword))
                            if elem varname reserved then fail "a reserved name was used!" else return ()
                            many1 space
                            vartype <- someType <?> "type"
                            assigned <- try (many1 space >> (literalOrVariable vars <|> (valMethodCall vars))) <|> (return (Lit NULL))
                            many space
                            punctuation
                            if typeCheck assigned vartype then  return () else fail ("Wrong Type assigned to " ++ varname)
                            return (DeclareVar(Variable(varname,vartype),assigned),Just(Variable(varname,vartype)))
        increment = do incremented <- choice (map numericVar vars)
                       space
                       try (string "got one more")
                       punctuation
                       return (Increment(incremented),Nothing)
        decrement = do decremented <- choice (map numericVar vars)
                       space
                       string "got one less"
                       return (Decrement(decremented),Nothing)
        numericVar (Variable(vname,Number)) = do try (string vname)
                                                 return (Variable(vname,Number))
        numericVar _ = fail "Tried to (de/in)crement an unknown variable"
        insMethodCall = do string "I remembered "
                           call <- methodCall vars <?> "method call"
                           return (InsMethodCall call, Nothing)



assignKeyword = choice $ map (\s -> try $ string s) ["has","is","likes","are","like","was"]
-- I'm thinking about giving up.

returnInstruction ret vars = do string "Then you get" -- this is a special one
                                many1 space
                                res <- (do  res <- literalOrVariable vars
                                            punctuation
                                            return res) <|> valMethodCall vars
                                if typeCheck res ret then return (Return res) else fail "returning wrong type"



valMethodCall args = do res <- methodCall args --for when a method call should be treated as a value
                        return (ValMethodCall(res))
methodCall vars = do methodName <- manyTill (letter <|> space) (lookAhead punctuation <|> lookAhead (try $ string "using"))
                     args <- (try $ string "using " >> args) <|> return []
                     punctuation
                     -- in both cases: Instruction "I remembered <methodname> <args>." and "<smth> <assign> <methodname> <args>."
                     -- methodCall should be ended with a punctuation
                     -- so: arguments can't be another methodCall (explicitly)
                     return (MethodCall(methodName,args))

  where args = do res <- literalOrVariable vars
                  rest <- ((string " and " >> args) <|> return [])
                  return (res:rest)

--TODO: methodCall doesn't check if the method is known or if the arguments are correct!


literalOrVariable vars = try add <|> try substract <|> try multiply <|> try divide <|> try wrapLiteral <|> try someVar  -- vars should have already been declared
  where wrapLiteral = do res <- try literal
                         return (Lit res)
        someVar = choice (map variable vars) <?> "known variable"
        variable v = do n <- string (getName v)
                        return (Var v)
        getName (Variable(n,_)) = n
        operator = choice $ map (\s -> try $ string s) ["plus","and","added to","minus","without","times","multiplied with","divided by"]
        add = do first <- wrapLiteral <|> try someVar
                 space
                 choice $ map (\s -> try $ string s) ["plus","and","added to"]
                 space
                 second <- literalOrVariable vars
                 if isNumber first && isNumber second then return (Add(first,second)) else fail "tried to add non-number values"
        substract = do first <- wrapLiteral <|> try someVar
                       space
                       choice $ map (\s -> try $ string s) ["minus","without"]
                       space
                       second <- literalOrVariable vars
                       if isNumber first && isNumber second then return (Substract(first,second)) else fail "tried to substract non-number values"
        multiply = do first <- wrapLiteral <|> try someVar
                      space
                      choice $ map (\s -> try $ string s) ["times","multiplied with"]
                      space
                      second <- literalOrVariable vars
                      if isNumber first && isNumber second then return (Multiply(first,second)) else fail "tried to multiply non-number values"
        divide = do first <- wrapLiteral <|> try someVar
                    space
                    string "divided by"
                    space
                    second <- literalOrVariable vars
                    if isNumber first && isNumber second then return (Add(first,second)) else fail "tried to divide non-number values"
        isNumber (Var(Variable(_,Number))) = True
        isNumber (Lit(N _)) = True
        isNumber (Add (_,_)) = True -- if an atom in a complex calculation is not numeric, it will be picked when parsing this atom
        isNumber (Substract(_,_)) = True
        isNumber (Divide(_,_)) = True
        isNumber (Multiply(_,_)) = True
        isNumber _ = False

literal = nullLiteral <|> boolLiteral <|> numLiteral <|> charLiteral <|> stringLiteral <?> "literal"
  where nullLiteral = do try (string "nothing")
                         return NULL
        boolLiteral = do res <- try (string "correct" <|> string "right"
                                    <|> string "true" <|> string "yes"
                                    <|> string "false" <|> string "incorrect"
                                    <|> string "wrong" <|> string "no")
                         if elem res ["correct","right","true","yes"]
                            then return (B True)
                            else return (B False)
        charLiteral = do char '\''
                         res <- anyChar
                         char '\''
                         return (C res)
        numLiteral = do res1 <- many1 digit
                        res2 <- try (char '.' >> many1 digit) <|> return []
                        let res3 = if res2==[] then res1 else res1++['.']++res2
                        return (N (read res3))
        stringLiteral = do char '\"'
                           res <- manyTill (noneOf "\"") (char '\"')
                           return (W res)
