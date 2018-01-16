module MainFunctions where

import Haskell4Maths (var
                     , vars
                     , zerov)
import F2 (PolF2)
import Logic
import Transformations ( projection
                       , theta)
import Heuristics
import Subsumption   -- not in use
import Analizador    (parseFProp)
import Preprocessing ( dimacs2Pols
                     , formulas2Pols)
import Saturation    ( forgetVarKB
                     , saturateKB)
import System.Environment
import qualified Data.Set as S

forgetVarListKB :: (S.Set PolF2,[PolF2]) -> [S.Set PolF2]
forgetVarListKB (ps,[])   = []
forgetVarListKB (ps,v:vs) = (aux:(forgetVarListKB (aux,vs)))
                where aux = forgetVarKB v ps

forgetVarListKB' :: (S.Set PolF2,[PolF2]) -> Heuristics -> [S.Set PolF2]
forgetVarListKB' (ps,[])   h = []
forgetVarListKB' (ps,vs) h = (aux:(forgetVarListKB' (aux,vs') h))
                   where (v:vs') = h ps vs
                         aux = forgetVarKB v ps

saturateKBTrace :: (S.Set PolF2,[PolF2]) -> Heuristics -> [(S.Set PolF2,Bool)]
saturateKBTrace (ps,[])   h                 = [(ps,S.notMember 0 ps)]
saturateKBTrace (ps,v:vs) h | S.member 0 ps = [(ps,False)]
                            | otherwise     = ((ps,True):(saturateKBTrace (aux, h aux vs) h))
                               where aux    = forgetVarKB v ps

satCNF = do
 let f = "exDIMACS/easy/example1.txt"
 let h = frequency
 putStrLn ("The satisfactibility of instance " ++ f ++
           " solved by frequency heuristics is:")
 f' <- dimacs2Pols f
 let sol = saturateKB f' h
 print sol

satFORMULAS = do
 let f = "exFORMULAS/easy/example1.txt"
 let h = frequency
 putStrLn ("The satisfactibility of instance " ++ f ++
           " solved by frequency heuristics is:")
 f' <- formulas2Pols f
 let sol = saturateKB f' h
 print sol