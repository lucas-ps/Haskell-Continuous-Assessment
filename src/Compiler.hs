module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter


-- acomp functions
acomp :: AExp -> [Instr]

acomp (N a) = [LOADI a]

acomp (V a) = [LOAD a]

acomp (Plus a b) = do    
    let valA = acomp a
    let valB = acomp b
    valA ++ valB ++ [ADD]


-- bcomp functions
-- The second parameter contains the boolean value for which a jump is required, the third parameter contains the number of instructions to jump over.
bcomp :: BExp -> Bool -> Int -> [Instr] 

bcomp (Bc a) c d = 
    if a == c then
        [JMP d]
    else 
        []

bcomp (Not a) c d =
    bcomp (a) (not c) d

bcomp (Less a b) c d = do
    let valA = acomp a
    let valB = acomp b
    if c then
        valA ++ valB ++ [JMPLESS d]
    else
        valA ++ valB ++ [JMPGE d]

bcomp (And a b) c d = do
    let valB = bcomp b c d
    if c then 
        bcomp a False (length valB) ++ valB
    else 
        bcomp a False (length valB + d) ++ valB


-- ccomp functions
ccomp :: Com -> [Instr] 

ccomp (SKIP) = [JMP 1]

ccomp (Assign a b) = do
    let valB = acomp b
    valB ++ [STORE a]

ccomp (Seq a b)  = do
    let valA = ccomp a
    let valB = ccomp b
    valA ++ valB

ccomp (If a b c) = do
    let valB = ccomp b
    let valA = bcomp a False (length valB + 1)
    let valC = ccomp c
    valA ++ valB ++ [JMP (length valC)] ++ valC
                                                                
ccomp (While a b) = do
    let valB = ccomp b
    let valA = bcomp a False (length valB + 1)
    valA ++ valB ++ [JMP((- (length valB)) - (length valB))]
  
-- Testing
{-
main = do
    print $ acomp (Plus (N 5) (V "x"))
    print $ bcomp (Bc True) True 3
    print $ bcomp (Bc False) False 3
    print $ bcomp (Bc True) False 3
    print $ bcomp (Not (Bc False)) False 3
    print $ bcomp (And (Bc True) (Bc False)) True 3
    print $ bcomp (And (Bc False) (Bc True)) True 3
    print $ bcomp (And (Bc True) (Bc False)) False 3
    print $ bcomp (And (Bc False) (Bc True)) False 3
    print $ bcomp (Less (V "x") (N 5)) True 3
    print $ bcomp (And (Less (V "x") (N 5)) (Bc True)) False 3
    print $ bcomp (And (Bc False) (Less (V "x") (N 5))) True 3
    print $ bcomp (And (Bc False) (Less (V "x") (N 5))) False 3
    print $ ccomp (If (Less (V "u") (N 1))(Assign "u" (Plus (V "u") (N 1)))(Assign "v" (V "u")))
    print $ ccomp (While (Less (V "u") (N 1)) (Assign "u" (Plus (V "u") (N 1))))
-}