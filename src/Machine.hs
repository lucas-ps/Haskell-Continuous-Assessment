module Machine
(      
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec,
) where

import Data.Map
import Data.Maybe


-- Defining types for the module
type Vname = String
type Val = Int
type State = Map Vname Val
data Instr =
        LOADI Int
        | LOAD String
        | ADD
        | STORE String
        | JMP Int
        | JMPLESS Int
        | JMPGE Int
        deriving (Eq, Read, Show)
type Stack = [Int] -- New items added to beginning of the queue, top = last item.
type Config = (Int, State, Stack) -- (Program counter, Variables (state), Values (Stack)


-- Helper functions for iexec and exec
addFirstTwoElements :: Stack -> [Int]
addFirstTwoElements [] = []
addFirstTwoElements xs = -- Returns the original stack with the first two items added together
    if length xs >= 2
        then do
            let a = xs !! 0
            let b = xs !! 1
            let result = sum [a, b]
            [result]
    else
        []

compareTwoTopmostValues :: Stack -> Bool
compareTwoTopmostValues xs =
    if length xs < 2 
        then error "Less than 2 items in the provided stack"
    else do
        let x = last xs
        let y = last (init xs)
        if x < y
            then True
        else False

removeTwoTopmostValues :: Stack -> Stack
removeTwoTopmostValues xs =
    if length xs < 2 
        then error "Less than 2 items in the provided stack"
    else do
        let l1 = init xs
        let l2 = init l1
        l2

-- iexec functions for executing instructions
iexec :: Instr -> Config -> Config -- in the following, pc = program counter, a = first value used, b = 2nd value used

iexec (LOADI x) (pc, a, b) =  (pc + 1, a, x : b)

iexec (LOAD v) (pc, a, b)
    | isNothing(Data.Map.lookup v a) = (pc + 1, a, b)
    | otherwise =
        let val = fromJust (Data.Map.lookup v a)
        in (pc + 1, a, val : b)

iexec ADD (pc, a, b)
    | length b >= 2 = (pc + 1, a, addFirstTwoElements b)
    | otherwise = (pc + 1, a, b)

iexec (STORE v) (pc, a, b)    
    | length b == 0 = (pc + 1, a, b)
    | otherwise = 
        (pc + 1, (insert v(last b) a), init b)

iexec (JMP i) (pc, a, b) = (pc + i + 1, a, b)

iexec (JMPLESS i) (pc, a, b)
    | compareTwoTopmostValues b = 
        let newB = removeTwoTopmostValues b
        in (pc + i + 1, a, newB)
    | otherwise = 
        let newB = removeTwoTopmostValues b
        in (pc + 1, a, newB)

iexec (JMPGE i) (pc, a, b)
    | not (compareTwoTopmostValues b) = 
        let newB = removeTwoTopmostValues b
        in (pc + i + 1, a, newB)
    | otherwise = 
        let newB = removeTwoTopmostValues b
        in (pc + 1, a, newB)

exec :: [Instr] -> Config -> Config
exec tasks (pc, a, b)
        | length tasks <= pc = (pc, a, b)
        | otherwise = 
            let currentProgram = tasks !! pc
            in exec tasks (iexec (currentProgram) (pc, a, b))

-- Testing
{-
main = do

    print $ iexec (LOADI 5) (0, empty, [])
    print $ iexec (LOAD "v1") (0, fromList [("v1", 5)] , [])
    print $ iexec ADD (0, empty, [5, 6])
    print $ iexec (STORE "x") (0, empty, [5])
    print $ iexec (JMP 5) (0, empty, [])
    print $ iexec (JMPLESS 5) (0, empty, [5, 6])
    print $ iexec (JMPGE 5) (0, empty, [5, 6])
    print $ exec [LOADI 1, LOADI 2, ADD] (0, empty, [])
    print $ exec [LOADI 1, STORE "v1 ", LOADI 2, STORE "v2"] (0, empty, [])
-}