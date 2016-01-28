{-#LANGUAGE FlexibleContexts #-}
module FiMppParser(parseClassFile) where

import CommentRemoval
import General
import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Types
import System.IO
import Data.Maybe

parseClassFile path = parseFile classDeclaration path

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
                      className <- manyTill (letter <|> space <|> char '\'') punctuation
                      many space
                      paragraphs <- methods (superClass:interfaces)
                      string "Your faithful student, "
                      programmerName <-manyTill (letter <|> space <|> char '\'') punctuation
                      return (Class(superClass,interfaces,className,paragraphs,programmerName))
  where classNames = manyTill className (char ':')
        className = do many space
                       optional (string "and" >> many space) <?> "another name"
                       notFollowedBy (char ':') <?> "another name"
                       manyTill (letter <|> space <|> char '\'') (more <|> end)
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
                     ins <- (instructions (methodEnd methodReturns) reserved methodArguments) <|> return [NoInstruction]
                     string "That's all about"
                     many1 space
                     string name
                     many space
                     punctuation
                     many space
                     return Method {isMain = mainMethod,
                                    name = name,
                                    returned = methodReturns,
                                    arguments = methodArguments,
                                    orders = ins}
  where methodName = manyTill (letter <|> space <|> char '\'') ((lookAhead returns) <|> (lookAhead args) <|> (lookAhead $ try $ many space >> try punctuation) <?> "punctuation")
        returns = (try $ many1 space >> (string "with" <|> string "to get" <|> string "as"))
        args = (try $ many1 space >> string "using")
        arguments = manyTill argument (try $ lookAhead punctuation)
        argument = do many space
                      t <- someType
                      many space
                      name <- manyTill (letter <|> space <|> char '\'') ((lookAhead $ try punctuation) <|> (lookAhead $ try $ string " and"))
                      optional (string "and")
                      return (Variable(name,t))
        methodEnd methodReturns vars =  do res <- if (methodReturns/= NoType) then returnInstruction methodReturns vars else return NoInstruction
                                           many space
                                           lookAhead (string "That's all about")
                                           return [res]

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

instructions final reserved vars = (try $ final vars) <|>
                                 do (ins,var) <- try (instruction reserved vars)
                                    let nvars = if isJust var then (fromJust var):vars else vars
                                    many space
                                    rest <- remainingInstructions nvars
                                    return (if ins == NoInstruction then rest else (ins:rest))
  where remainingInstructions vars = (try $ final vars) <|> instructions final reserved vars

instruction reserved vars = try conditional
                            <|> try reassign
                            <|> try varDeclaration
                            <|> try increment
                            <|> try decrement
                            <|> try insMethodCall <?> "instruction"
  where varDeclaration = do string "Did you know that"
                            many1 space
                            varname <- manyTill (letter <|> space <|> char '\'') (try (many1 space >> assignKeyword))
                            if elem varname reserved then fail "a reserved name was used!" else return ()
                            many1 space
                            vartype <- someType <?> "type"
                            assigned <- try (many1 space >> (literalOrVariable vars <|> valMethodCall vars)) <|> (return (Lit NULL)) <?> "a value"
                            punctuation
                            if typeCheck assigned vartype
                              then  return (DeclareVar(Variable(varname,vartype),assigned),Just(Variable(varname,vartype)))
                              else fail ("Wrong Type assigned to " ++ varname)
        assignKeyword = choice $ map (\s -> try $ string s) ["has","is","likes","are","like","was"]
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
        insMethodCall = do try (string "I remembered ") <|> try (string "I would ")
                           call <- methodCall vars <?> "method call"
                           punctuation
                           return (InsMethodCall call, Nothing)
        reassign = do var <- choice (map variable vars) <?> "known variable"
                      many1 space
                      reassignKeyword
                      many1 space
                      assigned <- try (do res <- (literalOrVariable vars)
                                          lookAhead punctuation
                                          return res)
                                   <|> valMethodCall vars <?> "a value"
                      punctuation
                      if typeCheck assigned (getType var) then return (Reassign(var,assigned),Nothing) else fail ("Wrong type assigned to "++ getName var)
        variable v = do n <- string (getName v)
                        return (v)
        reassignKeyword = choice $ map (\s -> try $ string s) ["became","becomes","become","is now","are now", "now likes","now like","now is","now are"]
        getName (Variable(n,_)) = n
        getType (Variable(_,t)) = t
        conditional = do try (string "If") <|> try (string "When")
                         space
                         condition <- literalOrVariable vars
                         if typeCheck condition Boolean then return () else fail "A condition should have type bool"
                         optional (string " then")
                         punctuation
                         space
                         case1 <- instructions (\_ -> lookAhead (endIf <|> startElse)) reserved vars
                         isElse <- checker (startElse >> punctuation)
                         case2 <- if isElse
                                     then (space >> instructions (\_ -> lookAhead endIf) reserved vars)
                                     else return []
                         endIf
                         punctuation
                         return (If(condition,case1,case2),Nothing)
        endIf = do try (string "That's what I would do")
                   return [NoInstruction]
        startElse = do try  (string "Otherwise" <|> string "Or else")
                       return [NoInstruction]

-- I'm thinking about giving up.

returnInstruction ret vars = do string "Then you get" -- this is a special one
                                many1 space
                                res <- literalOrVariable vars <|> valMethodCall vars
                                punctuation
                                if typeCheck res ret then return (Return res) else fail "returning wrong type"
                                --return (Return (ValMethodCall(MethodCall("Placeholder", map (\v -> Var v) vars))))



valMethodCall args = do res <- methodCall args --for when a method call should be treated as a value
                        return (ValMethodCall(res))
methodCall vars = do methodName <- manyTill (letter <|> space <|> char '\'') (lookAhead punctuation <|> lookAhead (try $ string " using"))
                     args <- (try $ string " using " >> args) <|> return []
                     lookAhead punctuation
                     -- in both cases: Instruction "I remembered <methodname> <args>." and "<smth> <assign> <methodname> <args>."
                     -- methodCall should be ended with a punctuation
                     -- reasons: arguments can't be another methodCall (explicitly), it would cause confusion
                     return (MethodCall(methodName,args))
  where args = do res <- literalOrVariable vars
                  rest <- ((string " and " >> args) <|> return [])
                  return (res:rest)

--TODO: methodCall doesn't check if the method is known or if the arguments are correct!


literalOrVariable vars = try add
                     <|> try substract
                     <|> try multiply
                     <|> try divide
                     <|> try con
                     <|> try alt
                     <|> try wrapLiteral
                     <|> try someVar  -- vars should have already been declared
  where wrapLiteral = do res <- try literal
                         return (Lit res)
        someVar = choice (map variable vars) <?> "known variable"
        variable v = do n <- try (string (getName v))
                        return (Var v)
        getName (Variable(n,_)) = n
        operator = choice $ map (\s -> try $ string s) ["plus","and","added to","minus","without","times","multiplied with","divided by"]
        add = do first <- wrapLiteral <|> try someVar
                 space
                 choice $ map (\s -> try $ string s) ["plus","and","added to"]
                 space
                 second <- literalOrVariable vars
                 if typeCheck first Number && typeCheck second Number
                    then return (Math(Add(first,second)))
                    else fail "tried to add non-number values"
        substract = do first <- wrapLiteral <|> try someVar
                       space
                       choice $ map (\s -> try $ string s) ["minus","without"]
                       space
                       second <- literalOrVariable vars
                       if typeCheck first Number && typeCheck second Number
                          then return (Math(Substract(first,second)))
                          else fail "tried to substract non-number values"
        multiply = do first <- wrapLiteral <|> try someVar
                      space
                      choice $ map (\s -> try $ string s) ["times","multiplied with"]
                      space
                      second <- literalOrVariable vars
                      if typeCheck first Number && typeCheck second Number
                         then return (Math(Multiply(first,second)))
                         else fail "tried to multiply non-number values"
        divide = do first <- wrapLiteral <|> try someVar
                    space
                    string "divided by"
                    space
                    second <- literalOrVariable vars
                    if typeCheck first Number && typeCheck second Number
                       then return (Math(Divide(first,second)))
                       else fail "tried to divide non-number values"
        con = do first <- wrapLiteral <|> try someVar
                 space
                 string "and"
                 space
                 second <- literalOrVariable vars
                 if typeCheck first Boolean && typeCheck second Boolean
                    then return (Logic(And(first,second)))
                    else fail "tried to AND non-boolean values"
        alt = do first <- wrapLiteral <|> try someVar
                 space
                 string "or"
                 space
                 second <- literalOrVariable vars
                 if typeCheck first Boolean && typeCheck second Boolean
                    then return (Logic(Or(first,second)))
                    else fail "tried to OR non-boolean values"


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
