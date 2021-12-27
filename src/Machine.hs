module Machine
(      
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where

import Data.Map


-- Defining types for the module
type Vname = String
type Val = Int
type State = Map Vname Val
data Instr =
        LOADI
        | LOAD
        | ADD
        | STORE
        | JMP
        | JMPLESS
        | JMPGE
        deriving (Eq, Read, Show)
type Stack = [Int]
type Config = (Int, State, Stack) -- (Program counter, Variables (state), Values (Stack)


-- Helper functions for iexec and exec
push :: Int -> Stack -> Stack -- Adds specified int to the top of the stack
push value xs = value : xs

pop :: Stack -> Int -- Returns item at the stack's head, returns nothing if list is empty.
pop [] = Nothing
pop xs = head xs


-- iexec functions for executing instructions
iexec :: Instr -> Config -> Config -- in the following, pc = program counter, a = first value used, b = 2nd value used

iexec (LOADI x) (pc, a, b) =  (pc + 1, a, push x b)

iexec (LOAD v) (pc, a, b)
    | Data.Map.null b = (pc + 1, a, b)
    | isNothing(Data.Map.lookup v var) = (pc + 1, a, b)
    | otherwise =
        let value = Data.Map.lookup v var
        in (pc + 1, a, push value b)

iexec ADD (pc, a, b)
    | length b <= 1 = (pc + 1, a, b)
    | otherwise = (pc + 1, b, sum c)

iexec (STORE v) (pc, a, b)
iexec (JMP i) (pc, a, b)
iexec (JMPLESS i) (pc, a, b)
iexec (JMPGE i) (pc, a, b)

--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec = undefined