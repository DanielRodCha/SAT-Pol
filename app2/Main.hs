module Main where

import Preprocessing (dimacs2Pols)
import Saturation (saturateKB)
import Heuristics

import System.Environment

-- | __(main f)__ is verified if the set of formulas in the
-- file /f/ were satisfiable. Otherwise, /(main f)/ would return False.

main = do
 [f,h] <- getArgs
 putStrLn ("The satisfactibility of instance " ++ f ++
           " solved by " ++ h ++ " heuristics is:")
 f' <- dimacs2Pols f
 let sol = saturateKB f' (aux h)
 print sol
 return sol

aux :: String -> Heuristics
aux "frequency"   = frequency
aux "monomialOrd" = monomialOrd
aux "revFreq"     = revFreq
aux "sizePol"     = sizePol
aux "freqSize"    = freqSize
aux "numVars"     = numVars
aux "freqNumVars" = freqNumVars