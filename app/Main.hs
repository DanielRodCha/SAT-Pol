module Main where

import Preprocessing (formulas2Pols)
import Saturation (saturateKB)
import Heuristics

import System.Environment

-- | __(main f)__ is verified if the set of formulas in DIMACS format in the
-- file /f/ were satisfiable. Otherwise, /(main f)/ would return False.

main = do
 let f = "exFORMULAS/easy/example1.txt"
 let h = frequency
 putStrLn ("The satisfactibility of instance " ++ f ++
           " solved by frequency heuristics is:")
 f' <- formulas2Pols f
 let sol = saturateKB f' h
 print sol
 return sol