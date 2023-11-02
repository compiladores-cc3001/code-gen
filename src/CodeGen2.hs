{-
  Generate 3-addresss Intermediate Code
  Second version, using the State monad to pass the names supply
-}
module CodeGen2 where

import           AST
import           IR
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State (State)
import qualified Control.Monad.State as State


-- symbol map identifiers to temporaries
type Table = Map Ident Temp

-- the "supply" for generating temporaries and labels
-- counter for temporaries and labels
type Supply = (Int, Int)   

-- get a new temporary
newTemp :: State Supply Temp
newTemp 
  = do (temps,labels) <- State.get
       State.put (temps+1, labels)
       return ("t"++show temps)

-- get a new label
newLabel :: State Supply Label 
newLabel
  = do (temps,labels) <- State.get
       State.put (temps, labels+1)
       return ("L"++show labels)


-- get several temporaries
newTemps :: Int -> State Supply [Temp]
newTemps n | n > 0 = do
               t <- newTemp
               ts <- newTemps (n-1)
               return (t:ts)
           | otherwise = return []

---------------------------------------------------------------------------

-- translate an expression
transExpr :: Expr -> Table -> Temp -> State Supply [Instr]
transExpr (Var x) tabl dest
  = case Map.lookup x tabl of
      Just temp -> return [MOVE dest temp]
      Nothing -> error "undefined variable"

transExpr (Num n) tabl dest 
  = return [MOVEI dest n]

transExpr (Op op e1 e2) tabl dest
  = do temp1 <- newTemp 
       temp2 <- newTemp 
       code1 <- transExpr e1 tabl temp1 
       code2 <- transExpr e2 tabl temp2
       return (code1 ++ code2 ++ [OP op dest temp1 temp2])

transExpr (Fun id args) tabl dest
  = do (code, temps) <- transArgs args tabl
       return (code ++ [CALL dest id temps])

-- translate functions arguments;
-- each one gets a new temporary
transArgs :: [Expr] -> Table -> State Supply ([Instr], [Temp])
transArgs [] tabl = return ([], [])
transArgs (exp:exps) tabl
      = do temp <- newTemp 
           code <- transExpr exp tabl temp 
           (code', temps') <- transArgs exps tabl
           return (code++code', temp:temps')


-- translate a statement
transStm :: Stm -> Table -> State Supply [Instr]
transStm (Assign var expr) tabl
  = case Map.lookup var tabl of
      Nothing -> error "undefined variable"
      Just dest -> transExpr expr tabl dest
                      
-- translate an if-then
transStm (If cond stm1) tabl 
  = do ltrue  <- newLabel 
       lfalse <- newLabel 
       code0  <- transCond cond tabl ltrue lfalse 
       code1  <- transStm stm1 tabl
       return (code0 ++ [LABEL ltrue] ++
               code1 ++ [LABEL lfalse])

-- translate an if-then-else
transStm (IfElse cond stm1 stm2) tabl
  = do ltrue <- newLabel 
       lfalse <- newLabel 
       lend <- newLabel 
       code0 <- transCond cond tabl ltrue lfalse 
       code1 <- transStm stm1 tabl 
       code2 <- transStm stm2 tabl 
       return (code0 ++
               [LABEL ltrue] ++ code1 ++
               [JUMP lend, LABEL lfalse] ++ code2 ++
               [LABEL lend]
              )
-- translate a while statement
transStm  (While cond stm) tabl =
  do lcond <- newLabel
     lbody <- newLabel
     lend <- newLabel
     code1 <- transCond cond tabl  lbody lend
     code2 <- transStm stm tabl 
     return ([LABEL lcond] ++ code1 ++
             [LABEL lbody] ++ code2 ++
             [JUMP lcond, LABEL lend]
            )

-- translate the return statement
transStm (Return expr) tabl =
  do dest <- newTemp
     code <- transExpr expr tabl  dest
     return (code ++ [RETURN dest])

-- translate a block of statements
transStm (Block stms) tabl =
  transStmList stms tabl 

-- translate a simple condition
-- i.e. (expr1 relop expr2)
-- see the book and lectures to see how to handle
-- more general conditions
transCond :: Expr -> Table -> Label -> Label -> State Supply [Instr]
transCond (Op rel e1 e2) tabl ltrue lfalse 
  | rel == Lt || rel == Lteq || rel == Eq =
      do temp1 <- newTemp
         temp2 <- newTemp 
         code1 <- transExpr e1 tabl temp1
         code2 <- transExpr e2 tabl temp2
         return ( code1 ++ code2 ++
                  [COND temp1 rel temp2 ltrue lfalse] )


-- translate a list of statements
-- translate individual statements and join the resulting instructions
transStmList :: [Stm] -> Table -> State Supply [Instr]
transStmList [] tabl = return []
transStmList (stm:rest) tabl = do
  code1 <- transStm stm tabl 
  code2 <- transStmList rest tabl
  return (code1 ++ code2)
  
        
----------------------------------------------------------------------------------

-- translate a function definition
transFunDef :: FunDef -> State Supply [Instr]
transFunDef (FunDef fun args locals body) 
  = do targs <- newTemps (length args)      -- temporaries for arguments
       tlocals <- newTemps (length locals)  -- temporaries for locals
       -- setup the symbol table
       let table = Map.fromList (zip args targs ++ zip locals tlocals)
       -- translate the body
       code <- transStmList body table
       -- return the code for the function
       return (LABEL fun : code)


