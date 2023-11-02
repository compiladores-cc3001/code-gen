{-
  Main module with code generation examples

  Pedro Vasconcelos, 2023
-}
module Main where

import AST
import IR
-- choose one version of the code gen module
import CodeGen2
-- import CodeGen3 
-- NB: version 1 doesn't use the same API; try it on the REPL
import qualified Data.Map as Map
import           Control.Monad.State (State)
import qualified Control.Monad.State as State

-- starting values for temporary and label counters
initialSupply :: Supply
initialSupply = (0, 0)

-- run a code generation action with initial supply
runCodeGen :: State Supply [Instr] -> [Instr]
runCodeGen gen = State.evalState gen initialSupply


main :: IO ()
main = do
  -- Example 1
  print example1
  printIR (runCodeGen (do dest <- newTemp
                          transExpr example1 Map.empty dest))
  
  -- Example 2
  line  
  print example2
  printIR (runCodeGen (do tx <- newTemp
                          ty <- newTemp
                          let tabl = Map.fromList [("x", tx), ("y", ty)]
                          transStm example2 tabl ))
  -- Example 3
  line  
  print example3
  printIR (runCodeGen (do ta <- newTemp
                          tb <- newTemp
                          tr <- newTemp
                          let tabl = Map.fromList [("a", ta), ("b", tb), ("r",tr)]
                          transStm example3 tabl ))
  -- Example 4
  line
  print example4
  printIR (runCodeGen (do result <- newTemp
                          transExpr example4 Map.empty result))
  

-- print an horizontal line
line :: IO () 
line = putStrLn (replicate 40 '-')

-- print a list of IR instructions
printIR :: [Instr] -> IO ()
printIR = mapM_ print


-- examples ----------------------------------------------------------------
example1 :: Expr
example1
  = Op Plus (Op Mult (Num 2) (Num 3)) (Num 1)

example2 :: Stm
example2
  = IfElse
    (Op Lt (Var "x") (Num 0))
    (Assign "y" (Num 1))
    (Assign "y" (Num 2))

example3 :: Stm
example3
  = While (Op Lt (Num 0) (Var "b"))
    ( Block [ Assign "r" (Op Mod (Var "a") (Var "b"))
            , Assign "a" (Var "b")
            , Assign "b" (Var "r")
            ]
    )

example4 :: Expr
example4
  = Op Plus
    (Fun "f" [Op Minus (Num 1) (Num 2)])
    (Fun "g" [Op Mult (Num 3) (Num 4)])


example5 :: FunDef
example5 = FunDef "max3" ["x","y","z"] ["m"]
           [ Assign "m" (Var "x")
           , IfElse (Op Lt (Var "x") (Var "y"))
             (Assign "m" (Var "y"))
             (Assign "m" (Var "x"))
           , IfElse (Op Lt (Var "m") (Var "z"))
             (Assign "m" (Var "z"))
             (Assign "m" (Var "m"))
           , Return (Var "m")
           ]
