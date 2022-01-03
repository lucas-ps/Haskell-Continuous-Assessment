module Main where

import System.Environment
import Compiler
import Interpreter

main :: IO ()
main = do
    args <- getArgs
    if length args == 0
        then print $ "Error: No arguments provided"
    else if length args > 1
        then print $ "Error: More than one argument provided"
    else 
        let command = head args
        in print(command)

    -- todo: get read working, currently returns io error? not sure how to fix or whether read is the correct thing to use
    -- runhaskell -isrc "/Users/lucas/OneDrive - University of Exeter/Year 2/CL and R/Coursework/app/Main.hs" "Assign /"x/" (Plus (N 5) (N 3))"`