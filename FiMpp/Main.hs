import FiMppParser
import Types

type Scope = (Variable,Literal)

runClass :: Class -> [Literal]
runClass (Class(_,_,_,methods,_)) = aux methods []
  where aux [] res = reverse res
        aux (m:ms) res = aux ms (if isMain m then (runMethod m [] methods):res else res)

runMethod ::  [Method] -> Method -> [Literal] -> Scope
runMethod methods method args = runInstructions createScope (orders method)
  where createScope = zip (arguments method) args
        runInstructions scope [NoInstruction] = NULL
        runInstructions scope [Return smth] = runReturn methods scope smth
        runInstructions scope (i:is) = runInstructions (runInstruction methods scope i) is

runReturn :: [Method] -> Scope -> VarOrLiteral -> Literal
runReturn methods scope ret = aux ret
  where aux (Var var) = getVarVal scope var
        aux (Lit literal) = literal
        aux smth =

runInstruction :: [Method] -> Scope -> Instruction -> Scope
runInstruction methods scope i = undefined

getVarVal :: Scope -> Variable -> Literal
getVarVal [] _ = fail "Lorem ipsum" -- the parser will prevent this from happening
getVarVal ((var1,lit):rest) var2 = if var1==var2 then lit else getVarVal rest var2

evaluate :: Literal -> Literal
evaluate
