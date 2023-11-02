{-
  Data types for 3-addresss Intermediate code
  Pedro Vasconcelos, 2022-23
-}
module IR where

-- for simplicity, we use the same binary operators
-- as in theabstract syntax
import AST (BinOp(..))

-- temporaries and labels are just strings
type Temp  = String
type Label = String

data Instr
  = MOVE Temp Temp                -- t1 := t2
  | MOVEI Temp Int                -- t  := n
  | OP BinOp Temp Temp Temp       -- t3 := t1 op t2
  | OPI BinOp Temp Temp Int       -- t2 := t1 op n
  | LABEL Label                   -- define label
  | JUMP Label                    -- unconditional jump
  | COND Temp BinOp Temp Label Label  -- conditional jump
  | CALL Temp Label [Temp]         -- call a function
  | RETURN Temp                    -- return from a function
  deriving (Eq, Show)

