module Main where

import Preprocessing (dimacs2Pols)
import Saturation (saturateKB)
import Heuristics

import System.Environment

-- | __(main f)__ is verified if the set of formulas in the
-- file /f/ were satisfiable. Otherwise, /(main f)/ would return False.

main = do
 let f = "exDIMACS/medium/exampleSat2.txt"
 let h = frequency
 putStrLn ("The satisfactibility of instance " ++ f ++
           " solved by frequency heuristics is:")
 f' <- dimacs2Pols f
 let sol = saturateKB f' h
 print sol
 return sol