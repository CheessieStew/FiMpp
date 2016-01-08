module Types(Method(..),VarType(..),Instruction(..),actualType,legalTypes) where


data Method = Method {isMain :: Bool,
                      name :: String,
                      returned :: VarType,
                      arguments :: [(String,VarType)],
                      root :: Instruction }
  deriving (Show)

data VarType = Number | Character | Boolean | Word | Many VarType | None
  deriving (Show,Eq)

data Instruction = Placeholder
  deriving (Show)


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
actualType _ = None
