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


--TODO Task 1.6
type Config = ()

--TODO Task 1.7
iexec :: Instr -> Config -> Config
iexec = undefined 

--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec = undefined