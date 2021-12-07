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

--TODO Task 1.1
type Vname = ()
--TODO Task 1.2
type Val = ()
--TODO Task 1.3
type State = ()

--TODO Task 1.4
data Instr =
        IUndefined
        deriving (Eq, Read, Show)

--TODO Task 1.5
type Stack = ()

--TODO Task 1.6
type Config = ()

--TODO Task 1.7
iexec :: Instr -> Config -> Config
iexec = undefined 

--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec = undefined