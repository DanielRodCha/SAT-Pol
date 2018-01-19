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

saturateKBSizeTrace :: (S.Set PolF2,[PolF2]) -> Heuristics -> [((PolF2,Int),Bool)]
saturateKBSizeTrace (ps,[])   h                 = [((0,S.size ps),S.notMember 0 ps)]
saturateKBSizeTrace (ps,v:vs) h | S.member 0 ps = [((v,S.size ps),False)]
                                | otherwise     = (((v,S.size ps),True):(saturateKBSizeTrace (aux, h aux vs) h))
                               where aux    = forgetVarKB v ps

satFORMULAS f h = do
 putStrLn ("The satisfactibility of instance " ++ f ++
           " solved by " ++ h ++ " heuristics is:")
 f' <- formulas2Pols f
 let sol = saturateKB f' (aux h)
 return sol

satCNF f h = do
 putStrLn ("The satisfactibility of instance " ++ f ++
           " solved by " ++ h ++ " heuristics is:")
 f' <- dimacs2Pols f
 let sol = saturateKB f' (aux h)
 return sol

aux :: String -> Heuristics
aux "frequency"   = frequency
aux "monomialOrd" = monomialOrd
aux "revFreq"     = revFreq