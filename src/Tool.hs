-- | This module aims to provide an efficient tool to solve the SAT
-- problem. Both the algorithm and the implementation have been carried out by
-- the researchers from the department of Computer Science and Artificial
-- Intelligence at the University of Seville.

module Tool
    ( tool
    , deltaRule
    , deltaRule1Step
    , next
    , toolTrace
    ) where

import Data.List (nub,iterate,partition, foldl', union)

import PolAux
import PolExamples
import Examples
import Heuristics (heuristics)
--import LogicFunctions (example)

import Math.CommutativeAlgebra.Polynomial (mdivides,lm)

import qualified Data.Set as S

-------------------------------------------------------------------------------

-- | (deltaRule v p1 p2) is the polynomial obtained from apply the independence
-- rule described in the paper [?] between polynomials /p1/ /p2/ respect the
-- variable /v/. It's important to note that as /v/ is the variable from wich we
-- derive, it won't occurs in the output polynomial. For example,
-- 
-- >>> deltaRule x1 1 1
-- 1
-- >>> deltaRule x1 1 0
-- 0
-- >>> deltaRule x1 x1 x1
-- 1

-- x11x28x41+x11x28+x28x41+x28+1
-- x11x68x77+x68x77+1

deltaRule :: PolF2 -> PolF2 -> PolF2 -> PolF2
deltaRule p a1 a2 = expTo1 (aux + a1a2 + aux2)
  where da1       = deriv a1 p
        da2       = deriv a2 p
        a1a2      = expTo1 (a1*a2)
        a1da2     = expTo1 a1*da2
        a2da1     = expTo1 a2*da1
        da1da2    = expTo1 da1*da2
        aux       = expTo1 (a1da2 + a2da1 + da1da2)
        aux2      = expTo1 (a1a2*aux)
        
-------------------------------------------------------------------------------

-- | __(deltaRuleMiniStep v p ps acum)__ is the union of the set /accum/, which
-- ramains invariant, and the set of polynomials obtained after applying
-- deltaRule respect /v/ between polynomial /p/ and every polynomial from set
-- /pps/, including itself. However, to improve efficiency the process is
-- terminated if a zero occurs. This is because a zero in the polynomial
-- set is sufficient for the tool to return False. For example,
--
-- >>> deltaRuleMiniStep x1 x1 (S.fromList [x1,x1*x2]) (S.fromList [1]) 
-- fromList [x2,1]
-- >>> deltaRuleMiniStep x1 x1 (S.fromList [x1,x1+1]) (S.fromList [1])
-- fromList [0]

deltaRuleMiniStep :: PolF2 -> PolF2 -> S.Set PolF2 -> S.Set PolF2 -> S.Set PolF2
deltaRuleMiniStep v p ps acum | S.null ps = acum
                              | dR == 0   = S.fromList [0]
                              | otherwise = deltaRuleMiniStep v p ps'
                                             (S.insert dR acum)
  where (p',ps') = S.deleteFindMin ps
        dR       = deltaRule v p p'

-------------------------------------------------------------------------------

-- | __(deltaRule1Step v pps acum)__ is the union of the set /accum/, which
-- ramains invariant, and the set of polynomials obtained after applying
-- deltaRule respect /v/ between every polynomial from set /pps/. For example,
-- 
-- >>> deltaRule1Step x1 (S.fromList [x1]) (S.fromList [1]) 
-- fromList [1]
-- >>> deltaRule1Step x1 (S.fromList [x1,x1*x2,x1*x3]) (S.empty) 
-- fromList [x2x3,x2,x3,1]

deltaRule1Step :: PolF2 -> S.Set PolF2 ->
                  S.Set PolF2 -> S.Set PolF2
deltaRule1Step v pps acum | acum == set0 = set0
                          | S.null pps   = acum
                          | otherwise    = deltaRule1Step v ps
                                           (deltaRuleMiniStep v p pps acum)
  where (p,ps) = S.deleteFindMin pps -- A pair form by the minimal element of
                                       -- a set and the original set without
                                       -- it.

-------------------------------------------------------------------------------

-- | __(next v ps)__ is the set of polynomials obtained after applying
-- deltaRule respect /v/ between every polynomial from /ps/, except if a zero
-- is obtained, in which case the process is stopped and the calculation is not
-- continued. For example,
-- >>> next x2 (S.fromList [x2,x1*x2,x1+1])
-- fromList [x1,x1+1,1]
-- >>> next x1 (S.fromList [x1,x1+1,1])
-- fromList [0]

next :: PolF2 -> S.Set PolF2 -> S.Set PolF2
next v ps = deltaRule1Step v ps1 ps2
       where (ps1,ps2) = S.partition (\p -> elem v (vars p)) ps

-------------------------------------------------------------------------------                                   
-- | __(tool (ps,vvs))__ is verified if the original set of formulas which
-- polynomials from /ps/ came was satisfiable. Otherwise, the function will
-- return False if that set of formulas was unsatisfiable. Note that /vvs/ is
-- the set of variables which occurs in any polynomial from /ps/. For example,
--
-- >>> tool (S.fromList[1],[])
-- True
-- >>> tool (S.fromList[x1,x1+1],[x1])
-- False

tool :: (S.Set PolF2,[PolF2]) -> Bool
tool (ps,[])                   = S.notMember 0 ps
tool (ps,v:vs) | S.member 0 ps = False
               | otherwise     = tool (nextVPS, heuristics nextVPS vs)
                 where nextVPS = next v ps

-------------------------------------------------------------------------------
set0 :: S.Set PolF2
set0 = S.fromList [0]


toolTrace (ps,[])   = [(ps,0)]
toolTrace (ps,v:vs) | ps == set0 = [(ps,v)]
                    | otherwise  = ((nextVPs,v):(toolTrace (nextVPs,vs)))
                       where nextVPs = next v ps


p,q,s,t :: PolF2
p = var "p"
q = var "q"
s = var "s"
t = var "t"

-- p1,p7,p9,p10 :: PolF2
-- p1 = var "p1"
-- p7 = var "p7"
-- p9 = var "p9"
-- p10 = var "p10"

-- pa1,pa2 :: PolF2
-- pa1 = p1*p10+p1+1
-- pa2 = (p1*p7*p9+p1*p7+1)

