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
        getLast
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
addToBottom :: Int -> Stack -> Stack -- Adds specified int to the bottom of the stack, returns new stack
addToBottom value stack = value : stack

getVal :: String -> State -> Maybe Int
getVal  = Data.Map.lookup -- Gets the val for a mapped variable

addFirstTwoElements :: Stack -> [Int]
addFirstTwoElements [] = []
addFirstTwoElements (x:xs) = -- Returns the original stack with the first two items added together
    if length xs >= 2
        then do
            let a = xs !! 0
            let b = xs !! 1
            let result = sum [a, b]
            result : Prelude.drop 1 xs 
        else
            []

getLast :: Stack -> Int
getLast [] = error "Tried to get last item from empty stack"
getLast (x:xs) = last xs

getFirst :: Stack -> Int
getFirst [] = error "Tried to get first item from empty stack"
getFirst (x:xs) = head xs



-- iexec functions for executing instructions
iexec :: Instr -> Config -> Config -- in the following, pc = program counter, a = first value used, b = 2nd value used

iexec (LOADI x) (pc, a, b) =  (pc + 1, a, addToBottom x b)

iexec (LOAD v) (pc, a, b)
    | isNothing(getVal v a) = (pc + 1, a, b)
    | otherwise =
        let val = fromJust (getVal v a)
        in (pc + 1, a, addToBottom val b)

iexec ADD (pc, a, b)
    | length b >= 2 = (pc + 1, a, addFirstTwoElements b)
    | otherwise = (pc + 1, a, b)

iexec (STORE v) (pc, a, b)    
    | length b == 0 = (pc + 1, a, b) -- checks if there are any items in the stack
    | otherwise = 
        (pc + 1, insert v (getLast b) a, init b)

--iexec (JMP i) (pc, a, b)

--iexec (JMPLESS i) (pc, a, b)

--iexec (JMPGE i) (pc, a, b)

--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec = undefined

-- Testing

main = do
    
    print $ iexec (LOADI 5) (0, empty, [])
    print $ iexec (LOAD "v1") (0, fromList [("v1", 5)] , [])
    print $ iexec ADD (0, empty, [5, 6])
    print $ iexec (STORE "x") (0, empty, [5])
    --print $ iexec (JMP 5) (0, empty, [])
    --print $ iexec (JMPLESS 5) (0, empty, [5, 6])
    --print $ iexec (JMPGE 5) (0, empty, [5, 6])
    --print $ exec [LOADI 1, LOADI 2, ADD] (0, empty, [])
    --print $ exec [LOADI 1, STORE "v1 ", LOADI 2, STORE "v2"] (0, empty, [])