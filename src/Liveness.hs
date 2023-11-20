{-# LANGUAGE BangPatterns #-}
{-
  Liveness analysis for IR Code
  Based on the presentation in Basics of Compiler Design, section 9.3
  Pedro Vasconcelos, 2021
-}
module Liveness where

import           IR
import           AST (BinOp(..))

import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Map (Map, (!))
import qualified Data.Map as Map

import           Data.List (nub) -- remove repeated values

-- indices for instructions
type Index = Int         

-- the dataflow mappings
type Gen  = Map Index (Set Temp)  -- generated temporaries
type Kill = Map Index (Set Temp)  -- killed temporaries
type Succ = Map Index [Index]     -- sucessors of each instruction

type In = Map Index (Set Temp)    -- in and out mappings
type Out = Map Index (Set Temp)


-- gen and kill sets for instructions
gen :: Instr -> Set Temp
gen (LABEL _)        = Set.empty
gen (MOVE x y)       = Set.singleton y
gen (MOVEI x k)      = Set.empty
gen (OP _ x y z)     = Set.fromList [y,z]
gen (OPI _ x y k)    = Set.singleton y
gen (JUMP _)         = Set.empty
gen (COND x _ y _ _) = Set.fromList [x,y]
gen (CALL x f args)  = Set.fromList args
gen (RETURN x)       = Set.singleton x

kill :: Instr -> Set Temp
kill (LABEL _)        = Set.empty
kill (MOVE x y)       = Set.singleton x
kill (MOVEI x k)      = Set.singleton x
kill (OP _ x y z)     = Set.singleton x
kill (OPI _ x y k)    = Set.singleton x
kill (JUMP _)         = Set.empty
kill (COND x _ y _ _) = Set.empty
kill (CALL x f args)  = Set.singleton x
kill (RETURN x)       = Set.empty


-- build Gen map from a list of instructions
generated :: [Instr] -> Gen
generated code
  = Map.fromList $ zip [1..] (map gen code)

-- build the Kill map from a list of instructions
killed :: [Instr] -> Kill
killed code
  = Map.fromList $ zip [1..] (map kill code)

-- build the sucessors map from a list of instructions
successors :: [Instr] -> Succ
successors code = Map.fromList $ zip [1..] $ zipWith next [1..] code
  where
    -- mapping from label names to instruction indices
    labels = Map.fromList [ (l,i) | (LABEL l, i) <- zip code [1..] ]
    -- successor for a single instruction
    next i (JUMP l) =  [labels!l]
    next i (COND x op y l1 l2) =  [labels!l1, labels!l2]
    next i (RETURN x) =  []
    next i _ =  [i+1]
                

-- liveness analysis; main entry point
-- results is number of iterations plus the in-out mappings
liveness :: [Instr] -> (Int, (In, Out))
liveness code
  = fixpointIters (iteration succ gen kill) (inp0, out0)
  where
    n = length code
    inp0 = Map.fromList $ zip [1..n] (repeat Set.empty)
    out0 = inp0
    gen = generated code
    kill = killed code
    succ = successors code


-- single iteration of the liveness analysis
iteration :: Succ -> Gen -> Kill -> (In, Out) -> (In, Out)
iteration succ gen kill (inp, out)
  = iterationAux ixs inp out
  where
     ixs = reverse (Map.keys inp)
     iterationAux [] inp out = (inp, out)
     iterationAux (i:ixs) inp out =
        let
           out_i = Set.unions [inp!j | j<-succ!i]
           in_i = ((gen!i) `Set.union` (out_i `Set.difference` (kill!i)))
           out'  = Map.insert i out_i out
           inp'  = Map.insert i in_i inp
         in iterationAux ixs inp' out'


-- compute iterations until a fixed point is reached
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint next initial = go initial
   where go x = let y = next x
                in if x==y then x else go y

-- same as above but also returns the number of iterations performed
fixpointIters :: Eq a => (a -> a) -> a -> (Int, a)
fixpointIters next initial = go initial 1
   where go x !k = let y = next x
                    in if x==y then (k, x) else go y (k+1)



-- compute the interference relation between temporaries
interference :: [Instr] -> Set (Temp, Temp)
interference code
  = Set.fromList [ (x,y)
                 | (i,instr) <- zip [1..] code
                 , x <- Set.toList (kill!i)
                 , y <- Set.toList (out!i) 
                 , x /= y, instr /= MOVE x y
                 ]
   where
     (_, (_, out)) = liveness code
     kill = killed code


---------------------------------------------------------------------------------
-- example programs
---------------------------------------------------------------------------------

example1 =
  [ MOVEI "a" 0
  , LABEL "L1"
  , MOVEI "t" 1
  , OP Plus "b" "a" "t"
  , OP Plus "c" "c" "b"
  , MOVEI "t" 2
  , OP Mult "a" "b" "t"
  , COND "a" Lt "n" "L1" "L2"
  , LABEL "L2"
  , RETURN "c"
  ]



-- fibonnacci(n)
example2 =
  [ MOVEI "a" 0
  , MOVEI "b" 1
  , MOVEI "z" 0
  , LABEL "loop"
  , COND "n" Eq "z" "end" "body"
  , LABEL "body"
  , OP Plus "t" "a" "b"
  , MOVE "a" "b"
  , MOVE "b" "t"
  , OPI Minus "n" "n" 1
  , MOVEI "z" 0
  , JUMP "loop"
  , LABEL "end"
  , RETURN "a"
  ]


-- integer square root(n)
example3 =
  [ MOVEI "i" 0
  , MOVEI "j" 1
  , JUMP "cond"
  , LABEL "loop"
  , OP Plus "i" "i" "j"
  , LABEL "cond"
  , OP Mult "t" "i" "i"
  , COND "t" Lt "n" "loop" "end"
  , LABEL "end"
  , RETURN "i"
  ]

-- integer log2(n)
example4 =
  [ MOVEI "r" 0
  , MOVEI "t" 1
  , JUMP "cond"
  , LABEL "loop"
  , OPI Div "n" "n" 2
  , OPI Plus "r" "r" 1
  , LABEL "cond"
  , COND "t" Lt "n" "loop" "end"
  , LABEL "end"
  , RETURN "r"
  ]

  
-- GCD using Euclid's algorithm
example5 = [ MOVEI "z" 0
         , LABEL "loop"
         , COND "a" Lt "b" "next" "swap"
         , LABEL "swap"
         , MOVE "t" "a"
         , MOVE "a" "b"
         , MOVE "b" "t"
         , LABEL "next"
         , OP Minus "b" "b" "a"
         , COND "b" Eq "z" "end" "loop"
         , LABEL "end"
         , RETURN "a"
         ]

