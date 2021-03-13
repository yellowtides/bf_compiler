module Main where

import Compiler ( compile ) 
import InternalRepr

-- A baby example for how the compilation for the internal representation works
sampleProgram :: Program
sampleProgram = [
        AssignBlock "a" (ValE 2),
        AssignBlock "b" (ValE 1),
        AssignBlock "c" (ValE 3),
        IfBlock (BinOpE Eq (VarE "a") (BinOpE Add (VarE "b") (ValE 1))) [
            AssignBlock "a" (ValE 7)
        ] 
        [] 
        []
    ] 

main :: IO ()
main = print $ compile sampleProgram