{-
  Basic translation of expressions into 3-Address Intermediate Code

  Version 1: Explicit passing of temporary and name supply
  Pedro Vasconcelos, 2022-23
-}
module CodeGen1 where

import           AST
import           IR
import           Data.Map (Map)
import qualified Data.Map as Map

-- symbol table mapping variables to temporaries
type Table = Map Ident Temp

-- the "supply" for temporaries and labels
-- just two counters for names already used
type Supply = (Int,Int)  

initialSupply :: Supply
initialSupply = (0, 0)

-- generate a new temporary and new supply
newTemp :: Supply -> (Temp, Supply)
newTemp (temps,labels)
  = ("t"++show temps, (temps+1,labels))

-- generate a new label and new supply
newLabel :: Supply -> (Label, Supply)
newLabel (temps,labels)
  = ("L"++show labels, (temps,labels+1))


-- translate an expression
transExpr :: Expr -> Table -> Ident -> Supply -> ([Instr], Supply)
transExpr (Var x) table dest supply0
  = case Map.lookup x table of
      Just temp -> ([MOVE dest temp], supply0)
      Nothing -> error "invalid variable"

transExpr (Num n) table dest supply0
  = ([MOVEI dest n], supply0)

transExpr (Op op e1 e2) table dest supply0
  = let (temp1, supply1) = newTemp supply0
        (temp2, supply2) = newTemp supply1
        (code1, supply3) = transExpr e1 table temp1 supply2
        (code2, supply4) = transExpr e2 table temp2 supply3
        code = code1 ++ code2 ++ [OP op dest temp1 temp2]
    in (code, supply4)

transExpr _ table dest supply0
  = error "not implemented"

transStm
  = error "not implemented"
