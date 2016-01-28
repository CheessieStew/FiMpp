import FiMppParser
import Types
import Data.List
import Data.Maybe
import Data.Either
import System.Environment

type Scope = [(Variable,Literal)]
type Log = [String]

runClass :: Class -> [Literal]
runClass (Class(_,_,_,methods,_)) = aux methods []
  where aux [] res = reverse res
        aux (m:ms) res = aux ms (if isMain m then (runMethod ms m []):res else res)

runMethod ::  [Method] -> Method -> [Literal] -> Literal
runMethod methods method args = snd $ runInstructions methods createScope (orders method)
  where createScope = zip (arguments method) args

runInstructions :: [Method] -> Scope -> [Instruction] -> (Scope,Literal)
runInstructions _ scope [NoInstruction] = (scope,NULL)
runInstructions methods scope [Return smth] = (scope,evaluate methods scope smth)
runInstructions methods scope (i:is) = runInstructions methods newScope is
  where newScope =  runInstruction methods scope i

runInstruction :: [Method] -> Scope -> Instruction -> Scope
runInstruction methods scope i = aux i
  where aux NoInstruction = scope
        aux (Return _) = scope -- won't happen
        aux (DeclareVar (var,vol)) = (var,evaluate methods scope vol):scope
        aux (Increment var) = incr scope var []
        aux (Decrement var) = decr scope var []
        aux (Reassign (var,vol)) = reass scope var vol []
        aux (If(cond,case1,case2)) = if (evaluate methods scope cond == B True)
          then fst $ runInstructions methods scope case1
          else fst $ runInstructions methods scope case2
        incr [] _ _ = scope -- won't happen
        incr ((var1,val):rest) var2 acc = if var1==var2
                                            then acc++ ((var1,val + (N 1)):rest)
                                            else incr rest var2 ((var1,val):acc)
        decr [] _ _ = scope -- won't happen
        decr ((var1,val):rest) var2 acc = if var1==var2
                                            then acc++ ((var1,val - (N 1)):rest)
                                            else incr rest var2 ((var1,val):acc)
        reass [] _ _ _ = scope --won't happen
        reass ((var1,val):rest) var2 vol acc = if var1==var2
                                                 then acc++ ((var1,evaluate methods scope vol):rest)
                                                 else reass rest var2 vol ((var1,val):acc)
getVarVal :: Scope -> Variable -> Literal
getVarVal [] _ = NULL -- the parser will always prevent this from happening
getVarVal ((var1,lit):rest) var2 = if var1==var2 then lit else getVarVal rest var2

evaluate :: [Method] -> Scope -> VarOrLiteral -> Literal
evaluate methods scope vol = aux vol
  where aux (Lit lit) = lit
        aux (Var var) = getVarVal scope var
        aux (ValMethodCall (MethodCall(name,args))) = runMethod methods (fromJust $ find (checkName name) methods) (map (evaluate methods scope) args)
        aux (Math(Add (vol1,vol2))) = (aux vol1) + (aux vol2)
        aux (Math(Substract (vol1,vol2))) =  (aux vol1) - (aux vol2)
        aux (Math(Multiply (vol1,vol2))) = (aux vol1) * (aux vol2)
        aux (Math(Divide (vol1,vol2))) = (aux vol1) / (aux vol2)
        aux (Logic(And (vol1,vol2))) = (aux vol1) &&* (aux vol2)
        aux (Logic(Or (vol1,vol2))) = (aux vol1) ||* (aux vol2)
        checkName string method = string == name method
        (&&*) (B v1) (B v2) = B (v1 && v2)
        (&&*) _ _ = NULL
        (||*) (B v1) (B v2) = B (v1 && v2)
        (||*) _ _ = NULL

mainR path = do cl <- parseClassFile path
                if isRight cl then return (runClass $ fromRight cl) else fail (fromLeft cl)
  where fromRight (Right smth) = smth
        fromRight (Left _) = Error
        fromLeft (Right _) = ""
        fromLeft (Left smth) = show smth

main = do args <- getArgs
          if length args /=1 then fail "wrong number of parameters" else return ()
          cl <- parseClassFile (head args)
          if isRight cl then return (runClass $ fromRight cl) else fail (fromLeft cl)
  where fromRight (Right smth) = smth
        fromRight (Left _) = Error
        fromLeft (Right _) = ""
        fromLeft (Left smth) = show smth
