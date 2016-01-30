import FiMppParser
import Types
import Data.List
import Data.Maybe
import Data.Either
import System.Environment

type Scope = [(Variable,Literal)]
type Log = [String]

runClass :: Class -> [String]
runClass (Class(_,_,_,methods,_)) = aux methods []
  where aux [] res = concatMap (\s -> "\n":s) (reverse res)
        aux (m:ms) res = aux ms (if isMain m then (fst $ runMethod methods m []): res else res)

runMethod :: [Method] -> Method -> [Literal] -> ([String],Literal)
runMethod methods method args = aux $ runInstructionBlock methods [createScope] (orders method) []
  where createScope = zip (arguments method) args
        aux (_,b,c) = (b,c)

runInstructionBlock :: [Method] -> [Scope] -> [Instruction] -> [String] -> ([Scope],[String],Literal)
runInstructionBlock _ (s:ss) [NoInstruction] out = (ss, reverse out,NULL)
runInstructionBlock methods (s:ss) [Return smth] out = (ss, reverse newOut++out,ret)
  where (ret,newOut) = evaluate methods (s:ss) smth
runInstructionBlock methods scope (i:is) out = runInstructionBlock methods newScope is (newOut++out)
  where (newScope,newOut) = runInstruction methods scope i

runInstruction :: [Method] -> [Scope] -> Instruction -> ([Scope],[String])
runInstruction methods scope i = aux i
  where aux NoInstruction = (scope,[])
        aux (Return _) = (scope,[]) -- won't happen
        aux (DeclareVar (var,vol)) = (((var,fst $ evalVol vol): (head scope)) : (tail scope),snd $ evalVol vol)
        aux (Increment var) = (modifyFirst var (\n -> (n + (N 1) , [])) scope  [] [])
        aux (Decrement var) = (modifyFirst var (\n -> (n - (N 1) , [])) scope  [] [])
        aux (Reassign (var,vol)) = (modifyFirst var (\_ -> evaluate methods scope vol) scope  [] [])
        aux (If(cond,case1,case2)) = if (evalCond == B True)
                                    then aux2 $ runInstructionBlock methods ([]:scope) case1 (reverse out)
                                    else aux2 $ runInstructionBlock methods ([]:scope) case2 (reverse out)
          where (evalCond,out) = evaluate methods scope cond
                aux2 (a,b,_) = (a,b)
        aux (Say(vols)) = (scope, [concatMap (show.fst.(evaluate methods scope)) vols]) -- things to say will never be method calls, so they don't produce more output
        evalVol vol = evaluate methods scope vol
        modifyFirst what modifier [] acc1 acc2 = (reverse (acc1 : acc2),[]) -- shouldn't really happen
        modifyFirst what modifier ([]:ext) acc1 acc2 = modifyFirst what modifier ext [] (acc1:acc2) -- if it's not in the local scope, search in the external one
        modifyFirst what modifier (((var,val):rest):ext) acc1 acc2 = if what == var
                                                                     then ((reverse acc2) ++ ((((var,newVal) :rest)++acc1) : ext),out)
                                                                     -- the order in the local scope doesn't matter, but the order of layers matters
                                                                     else modifyFirst what modifier (rest:ext) ((var,val):acc1) acc2
          where (newVal,out) = modifier val

evaluate :: [Method] -> [Scope] -> VarOrLiteral -> (Literal,[String])
evaluate methods scope vol = aux vol
  where aux (Lit lit) = (lit,[])
        aux (Var var) = (getVarVal scope var,[])
        aux (ValMethodCall (MethodCall(methodName,args))) = (methodRes,concat moreOut ++ methodOut)
          where checkName string method = string == name method
                (methodRes,methodOut) = extr $ runMethod methods (fromJust $ find (checkName methodName) methods) evalArgs
                extr (out,lit) = (lit,out)
                (evalArgs,moreOut) = unzip $ (map (evaluate methods scope) args)
        aux (Math(Add (vol1,vol2))) = ((fst $ aux vol1) + (fst $ aux vol2),[])
        aux (Math(Substract (vol1,vol2))) =  ((fst $ aux vol1) - (fst $ aux vol2),[])
        aux (Math(Multiply (vol1,vol2))) = ((fst $ aux vol1) * (fst $ aux vol2),[])
        aux (Math(Divide (vol1,vol2))) = ((fst $ aux vol1) / (fst $ aux vol2),[])
        aux (Logic(And (vol1,vol2))) = ((fst $ aux vol1) &&* (fst $ aux vol2),[])
        aux (Logic(Or (vol1,vol2))) = ((fst $ aux vol1) ||* (fst $ aux vol2),[])
        (&&*) (B v1) (B v2) = B (v1 && v2)
        (&&*) _ _ = NULL
        (||*) (B v1) (B v2) = B (v1 && v2)
        (||*) _ _ = NULL
getVarVal :: [Scope] -> Variable -> Literal
getVarVal [] _ = NULL
getVarVal ([]:ext) var = getVarVal ext var
getVarVal (((var1,lit):rest):ext) var2 = if var1==var2 then lit else getVarVal (rest:ext) var2

mainR path = do cl <- parseClassFile path
                if isRight cl then putStr $ intercalate "\n" (runClass $ fromRight cl) else fail (fromLeft cl)
                putStr "\n"
  where fromRight (Right smth) = smth
        fromRight (Left _) = Error
        fromLeft (Right _) = ""
        fromLeft (Left smth) = show smth

main = do args <- getArgs
          if length args /=1 then fail "wrong number of parameters" else return ()
          cl <- parseClassFile (head args)
          putStr $ "running " ++ (head args) ++ "\n\n"
          if isRight cl then putStr (show $ runClass $ fromRight cl) else fail (fromLeft cl)
  where fromRight (Right smth) = smth
        fromRight (Left _) = Error
        fromLeft (Right _) = ""
        fromLeft (Left smth) = show smth
