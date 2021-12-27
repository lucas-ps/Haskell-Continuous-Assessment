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


--TODO Task 1.7
iexec :: Instr -> Config -> Config
iexec = undefined 

--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec = undefined