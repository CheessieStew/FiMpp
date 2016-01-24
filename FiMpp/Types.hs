module Types(Method(..),VarType(..),Instruction(..),Literal(..),VarOrLiteral(..),BoolOperator(..),Variable(..),actualType,legalTypes) where


data Method = Method {isMain :: Bool,
                      name :: String,
                      returned :: VarType,
                      arguments :: [Variable],
                      orders :: [Instruction] }
  deriving (Show)

data VarType = Number | Character | Boolean | Word | Many VarType | NoType
  deriving (Show,Eq)

data Literal = N Double | C Char | B Bool | W String | NULL
  deriving (Show,Eq)

data Variable = Variable (String,VarType)
  deriving (Show,Eq)

data VarOrLiteral = Var Variable | Lit Literal | Add (VarOrLiteral,VarOrLiteral) | Substract (VarOrLiteral,VarOrLiteral) | Multiply (VarOrLiteral,VarOrLiteral) | Divide (VarOrLiteral,VarOrLiteral)
  deriving (Show,Eq)

data Instruction = NoInstruction | Return VarOrLiteral | DeclareVar (Variable,VarOrLiteral) | Increment Variable | Decrement Variable
  deriving (Show,Eq)


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
