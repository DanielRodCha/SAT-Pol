module Main where

import Preprocessing (dimacs2Pols)
import Saturation (saturateKB)
import Heuristics

import System.Environment

-- | __(main f)__ is verified if the set of formulas in DIMACS format in the
-- file /f/ were satisfiable. Otherwise, /(main f)/ would return False.

main = do
 let f = "exDIMACS/medium/exampleSat3.txt"
 let h = frequency
 putStrLn ("The satisfactibility of instance " ++ f ++
           " solved by frequency heuristics is:")
 f' <- dimacs2Pols f
 let sol = saturateKB f' h
 print sol


-- aux :: Heuristics -> String
-- aux monomialOrd = "monomialOrd"
-- aux frequency = "frequency"
-- aux revFreq = "reverse frequency"
-- --showHeuristics ( _ _) = ""

-- main = sat


-- sat = do
--   putStrLn "Which SAT instance do you want to solve?"
--   f <- readLn
--   f' <- dimacs2Pols f
--   putStrLn "Which Heuristics do you want to use to?"
--   h <- readLn
--   return (saturateKB f' h)