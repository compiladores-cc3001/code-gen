{-
  Abstract Syntax Trees for a simple imperative language
  Pedro Vasconcelos, 2022-23
-}
module AST where

-- identifiers are just strings
type Ident = String

-- expressions
data Expr =
          -- identifiers
          Var Ident                 -- x, y, z
          -- constants 
          | Num Int                -- 1, 2, 3
          -- operators
          | Op BinOp Expr Expr     -- e1+e2, e1*e2, e1<e2, etc.
          -- function call
          | Fun Ident [Expr]       -- fun(e1,...en)
          deriving Show

-- binary operations
data BinOp = Plus | Minus | Mult | Div | Mod | Lt | Lteq | Eq | Neq
           deriving (Eq, Show)

--- statements
data Stm = Assign Ident Expr       -- var = expr
         | If Expr Stm             -- conditionals
         | IfElse Expr Stm Stm
         | While Expr Stm          -- while loops
         | Block [Stm]             -- blocks of statements
         | Return Expr             -- return from a function
         deriving Show


-- function definition
data FunDef = FunDef Ident [Ident] [Ident] [Stm]
            --        fun   (args)   locals  body
  deriving Show

