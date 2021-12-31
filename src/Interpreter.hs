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
    | GetResultB BExp
    deriving (Eq, Read, Show)

data Com =
      Assign Vname AExp
    | Seq Com Com
    | If BExp Com Com
    | While BExp Com
    | SKIP
    | GetResult Com
    deriving (Eq, Read, Show)


-- aval functions 
aval :: AExp -> State -> Val -- State = Variables

aval (N a) state = a -- Converts ints to vals, not sure if this is useful?

aval (V a) state = fromMaybe 0 (Data.Map.lookup a state) -- Gets value of variable provided

aval (Plus a b) state = do -- Method for adding provided a and b
    let valA = aval a state
    let valB = aval b state
    valA + valB


-- bval instructions
bval :: BExp -> State -> Bool

bval (GetResultB x) state = case x of 
                        (Less a b) -> bval (Less a b) state
                        (And a b) -> bval (And a b) state
                        (Not a) -> bval (Not a) state
                        (Bc a) -> a

bval (Less a b) state = 
    if (aval a state) < (aval b state)
        then True
    else False

bval (And a b) state = do
    let valA = bval (GetResultB a) state
    let valB = bval (GetResultB b) state
    if valA && valB then True else False
    
bval (Not a) state =
    let valA = bval (GetResultB a) state
    in not(valA)


-- Eval instructions
eval :: Com -> State -> State

eval (GetResult x) state = case x of 
                        (Assign v x) -> eval (Assign v x) state
                        (Seq c1 c2) -> eval (Seq c1 c2) state
                        (If b c1 c2) -> eval (If b c1 c2) state
                        (While b c) -> eval (While b c) state  
                        (SKIP) -> eval SKIP state

eval (Assign v x) state = -- v = variable, x = arithmatic expression
    let valX _ = Just(aval x state)
    in alter valX v state

eval SKIP state = state -- Do I need to incriment program counter?

eval (Seq c1 c2) stack =
    let newStack = eval (GetResult c1) stack 
    in eval (GetResult c2) newStack 

eval (If b c1 c2) state 
    | bval (GetResultB b) state = eval (GetResult c1) state
    | otherwise = eval (GetResult c2) state

eval (While b c) state =
    if bval (b) state 
        then 
            let newState = eval (GetResult c) state
            in eval (While b c) newState
    else state
            

-- Testing
{-
main = do

    print $ aval (Plus (N 3) (V "x")) (fromList [("x",2)])
    print $ bval (Less (N 3) (V "x")) (fromList [("x",0)])
    print $ eval SKIP (fromList [])
    print $ eval (Assign "x" (N 5)) (fromList [("x",0)])
    print $ eval (Seq (Assign "x" (N 5)) (Assign "x" (N 6))) (fromList [("x",0)])
    print $ eval (If (Less (V "x") (N 5)) (Assign "x" (N 6)) (SKIP)) (fromList [("x",4)])
    print $ eval (If (Less (V "x") (N 5)) (Assign "x" (N 6)) (SKIP)) (fromList [("x",10)])    
    print $ eval (While (Less (V "x") (N 5)) (Assign "x" (Plus (V "x") (N 1)))) (fromList [("x",0)])

 -}