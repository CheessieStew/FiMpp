module Types(Method(..),
             VarType(..),
             Instruction(..),
             Literal(..),
             VarOrLiteral(..),
             BoolOperator(..),
             Variable(..),
             MethodCall(..),
             Class(..),
             actualType,
             legalTypes,
             typeCheck) where

import Data.List

data Class = Class (String,[String],String,[Method],String)
  deriving (Eq)
instance Show Class where
  show (Class(address,interfaces,title,methods,author))
    = "Address: " ++ address ++"\n"
      ++ "Interfaces: " ++ (if interfaces==[]
                              then "empty"
                              else intercalate ", " interfaces)++ "\n"
      ++ "Title: " ++ title ++ "\n\n"
      ++ intercalate "\n\n" (map show methods) ++ "\n\n"
      ++ "Signed: " ++ author

data Method = Method {isMain :: Bool,
                      name :: String,
                      returned :: VarType,
                      arguments :: [Variable],
                      orders :: [Instruction] }
  deriving (Eq)
instance Show Method where
  show method = show (returned method) ++ " "
                ++ (if (isMain method) then "(main) " else "")
                ++ name method
                ++  "(" ++ intercalate ", " (map show (arguments method)) ++ ")"
                ++ "\n{\n" ++ intercalate "\n" (map (("\t"++).show) (orders method)) ++ "\n}"

data VarType = Number | Character | Boolean | Word | Many VarType | NoType
  deriving (Show,Eq)

data Literal = N Double | C Char | B Bool | W String | NULL
  deriving (Eq)
instance Show Literal where
  show (N v) =  show v
  show (C v) = show v
  show (B v) = show v
  show (W v) = show v
  show NULL = "nothing"

data Variable = Variable (String,VarType)
  deriving (Eq)
instance Show Variable where
  show (Variable(name,t)) =name++"::"++(show t)

data VarOrLiteral = Var Variable
                  | Lit Literal
                  | Add (VarOrLiteral,VarOrLiteral)
                  | Substract (VarOrLiteral,VarOrLiteral)
                  | Multiply (VarOrLiteral,VarOrLiteral)
                  | Divide (VarOrLiteral,VarOrLiteral)
                  | ValMethodCall (MethodCall)
  deriving (Eq)
instance Show VarOrLiteral where
  show (Var v) = show v
  show (Lit v) = show v
  show (Add (v1,v2)) = show v1 ++ "+" ++ show v2
  show (Substract (v1,v2)) = show v1 ++ "-" ++ show v2
  show (Multiply (v1,v2)) = show v1 ++ "*" ++ show v2
  show (Divide (v1,v2)) = show v1 ++ "/" ++ show v2
  show (ValMethodCall v) = show v

data Instruction = NoInstruction
                 | Return VarOrLiteral
                 | DeclareVar (Variable,VarOrLiteral)
                 | Increment Variable
                 | Decrement Variable
                 | InsMethodCall MethodCall
  deriving (Eq)
instance Show Instruction where
  show NoInstruction = "nope"
  show (Return v) = "return("++ show v ++ ")"
  show (DeclareVar(v,a)) = show v ++ ":=" ++ show a
  show (Increment v) = show v ++ "++"
  show (Decrement v) = show v ++ "--"
  show (InsMethodCall v) = show v

data MethodCall = MethodCall (String, [VarOrLiteral])
  deriving (Eq)
instance Show MethodCall where
  show (MethodCall (name,args)) = name++ "(" ++ intercalate ", " (map show args) ++ ")"

data BoolOperator = Equal (VarOrLiteral,VarOrLiteral)
                  | NotEqual (VarOrLiteral,VarOrLiteral)
                  | Greater (VarOrLiteral,VarOrLiteral)
                  | GreaterEqual (VarOrLiteral,VarOrLiteral)
                  | Less (VarOrLiteral,VarOrLiteral)
                  | LessEqual (VarOrLiteral,VarOrLiteral)
  deriving (Show,Eq)




legalTypes =
 ["number",
  "letter",
  "character",
  "logical",
  "argument",
  "logic",
  "word",
  "phrase",
  "sentence",
  "quote"]

actualType "number" = Number
actualType "letter" = Character
actualType "character" = Character
actualType "logical" = Character
actualType "argument" = Boolean
actualType "logic" = Boolean
actualType "word" = Word
actualType "phrase" = Word
actualType "sentence" = Word
actualType "quote" = Word
actualType _ = NoType

typeCheck (Lit (N _)) Number = True
typeCheck (Lit (C _)) Character = True
typeCheck (Lit (W _)) Word = True
typeCheck (Lit (B _)) Boolean = True
typeCheck (Lit NULL) _ = True
typeCheck (Var (Variable(_,t1))) t2 = t1==t2
typeCheck (ValMethodCall _) _ = True
 -- for now, I have no way of checking this, so assigning a number-returning method
 -- to a word variable will pass, though it should not
typeCheck _ _ = False
