module Interpreter
(
    AExp(..),
    BExp(..),
    Com (..),
    aval,
    bval,
    eval
) where

import Data.Map
import Data.Maybe
import Machine


-- Defining types for the module
data AExp =
      N Val
    | V Vname
    | Plus AExp AExp
    deriving (Eq, Read, Show)

data BExp =
      Bc Bool
    | Not BExp
    | And BExp BExp
    | Less AExp AExp
    deriving (Eq, Read, Show)

data Com =
      Assign Vname AExp
    | Seq Com Com
    | If BExp Com Com
    | While BExp Com
    | SKIP
    deriving (Eq, Read, Show)


-- Helper methods
getVal :: AExp -> State -> Val -- Returns needed val whether or not the provided item is a vname or a val
getVal (N val) state = val
getVal (V vname) state = fromMaybe 0 (Data.Map.lookup vname state)


-- AExp functions 
aval :: AExp -> State -> Val -- State = Variables

aval (N a) state = a -- Converts ints to vals, not sure if this is useful?

aval (V a) state = fromMaybe 0 (Data.Map.lookup a state) -- Gets value of variable provided

aval (Plus a b) state = do -- Method for adding provided a and b
    let valA = getVal a state
    let valB = getVal b state
    valA + valB


bval :: BExp -> State -> Bool

bval (Less a b) state = 
    if (getVal a state) < (getVal b state)
        then True
    else False

bval (And a b) state = do
    let valA = case a of 
                    (Less c d) -> bval (Less c d) state
                    (And c d) -> bval (And c d) state
                    (Not c) -> bval (Not c) state
                    (Bc c) -> c
    let valB = case b of
                    (Less c d) -> bval (Less c d) state
                    (And c d) -> bval (And c d) state
                    (Not c) -> bval (Not c) state
                    (Bc c) -> c
    if valA && valB then True else False
    
bval (Not a) state =
    let valA = case a of 
                    (Less c d) -> bval (Less c d) state
                    (And c d) -> bval (And c d) state
                    (Not c) -> bval (Not c) state
                    (Bc c) -> c   
    in not(valA)


eval :: Com -> State -> State
--eval (Assign v x) state -- v = variable, x = arithmatic expression
eval SKIP state = state
--eval (Seq a b) stack
--eval (If a b c) state
--eval (While c d) state

-- Testing
-- {-
main = do

    print $ aval (Plus (N 3) (V "x")) (fromList [("x",2)])
    print $ bval (Less (N 3) (V "x")) (fromList [("x",0)])
    print $ eval SKIP (fromList [])
    print $ eval (Assign "x" (N 5)) (fromList [("x",0)])
    print $ eval (Seq (Assign "x" (N 5)) (Assign "x" (N 6))) (fromList [("x",0)])
    print $ eval (If (Less (V "x") (N 5)) (Assign "x" (N 6)) (SKIP)) (fromList [("x",4)])
    print $ eval (If (Less (V "x") (N 5)) (Assign "x" (N 6)) (SKIP)) (fromList [("x",10)])    
    print $ eval (While (Less (V "x") (N 5)) (Assign "x" (Plus (V "x") (N 1)))) (fromList [("x",0)])

---}