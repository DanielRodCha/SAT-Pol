module Main where

import Preprocessing (formulas2Pols)
import Saturation (saturateKB)
import Heuristics

import System.Environment

-- | __(main f)__ is verified if the set of formulas in DIMACS format in the
-- file /f/ were satisfiable. Otherwise, /(main f)/ would return False.

main = do
 [f,h] <- getArgs
 putStrLn ("The satisfactibility of instance " ++ f ++
           " solved by " ++ h ++ " heuristics is:")
 f' <- formulas2Pols f
 let sol = saturateKB f' (aux h)
 print sol
 return sol

aux :: String -> Heuristics
aux "frequency"   = frequency
aux "monomialOrd" = monomialOrd
aux "revFreq"     = revFreq