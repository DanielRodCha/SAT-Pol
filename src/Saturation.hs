module Saturation where

import Haskell4Maths (var
                     , vars
                     , zerov)
import F2 (PolF2)
import Rule (independenceRule)
import Heuristics

import System.Environment
import qualified Data.Set as S

-- | __(independenceRuleAux v p ps acum)__ is the union of the set /accum/, which
-- ramains invariant, and the set of polynomials obtained after applying
-- independenceRule respect /v/ between polynomial /p/ and every polynomial from set
-- /pps/, including itself. However, to improve efficiency the process is
-- terminated if a zero occurs. This is because a zero in the polynomial
-- set is sufficient for the tool to return False. For example,

independenceRuleAux :: PolF2 -> PolF2 -> S.Set PolF2 ->
                            S.Set PolF2 -> S.Set PolF2
independenceRuleAux v p ps acum
  | S.null ps = acum
  | dR == 0   = S.fromList [0]
  | otherwise = independenceRuleAux v p ps' (S.insert dR acum)
                where (p',ps') = S.deleteFindMin ps
                      dR       = independenceRule v p p'

-- | __(independenceRuleKB v pps acum)__ is the union of the set /accum/, which
-- ramains invariant, and the set of polynomials obtained after applying
-- deltaRule respect /v/ between every polynomial from set /pps/. For example,
--
-- >>> [x1,x2,x3] = (map var ["x1","x2","x3"]) :: [PolF2]
-- >>> independenceRuleKB x1 (S.fromList [x1]) (S.fromList [1]) 
-- fromList [1]
-- >>> independenceRuleKB x1 (S.fromList [x1,x1*x2,x1*x3]) (S.empty) 
-- fromList [x2x3,x2,x3,1]
independenceRuleKB :: PolF2 -> S.Set PolF2 ->
                  S.Set PolF2 -> S.Set PolF2
independenceRuleKB v pps acum
  | acum == S.fromList [0] = S.fromList [0]
  | S.null pps   = acum
  | otherwise    = independenceRuleKB v ps
                   (independenceRuleAux v p pps acum)
      where (p,ps) = S.deleteFindMin pps

-- | For example,
-- >>> x1 = (var "x1") :: PolF2
-- >>> x2 = (var "x2") :: PolF2
-- >>> forgetVarKB x2 (S.fromList [x2,x1*x2,x1+1])
-- fromList [x1,x1+1,1]
-- >>> forgetVarKB x1 (S.fromList [x1,x1+1,1])
-- fromList [0]

forgetVarKB :: PolF2 -> S.Set PolF2 -> S.Set PolF2
forgetVarKB v ps = independenceRuleKB v ps1 ps2
       where (ps1,ps2) = S.partition (\p -> elem v (vars p)) ps

-- | For example,
--
-- >>> saturateKB (S.fromList[1],[]) monomialOrd
-- True
-- >>> x1 = (var "x1") :: PolF2
-- >>> saturateKB (S.fromList[x1,x1+1],[x1]) monomialOrd
-- False

saturateKB :: (S.Set PolF2,[PolF2]) -> Heuristics -> Bool
saturateKB (ps,[])   h                 = S.notMember 0 ps
saturateKB (ps,v:vs) h | S.member 0 ps = False
                       | otherwise     = saturateKB (aux, h aux vs) h
                         where aux     = forgetVarKB v ps

