-- | This module aims to provide an efficient tool to solve the SAT
-- problem. Both the algorithm and the implementation have been carried out by
-- the researchers from the department of Computer Science and Artificial
-- Intelligence at the University of Seville.

module ToolCount
    ( toolCount
    , toolLC
    , counting
    ) where

import Data.List (nub,iterate,partition, foldl', union)

import PolAux
import PolExamples
import Examples
import LogicFunctions (example)

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

-- | __(deltaRule1Step v pps acum)__ is the union of the set /accum/, which
-- ramains invariant, and the set of polynomials obtained after applying
-- deltaRule respect /v/ between every polynomial from set /pps/. For example,
-- 
-- >>> deltaRule1Step x1 (S.fromList [x1]) (S.fromList [1]) 0
-- (fromList [1],1)
-- >>> deltaRule1Step x1 (S.fromList [x1,x1*x2,x1*x3]) (S.empty) 0
-- (fromList [x2x3,x2,x3,1],6)

deltaRule1Step :: PolF2 -> S.Set (PolF2) -> S.Set (PolF2) ->
                  Integer -> (S.Set (PolF2),Integer)
deltaRule1Step v pps acum i | S.null pps = (acum,i)
                            | otherwise  = deltaRule1Step v ps dRMS i'
                          
  where (p,ps)   = S.deleteFindMin pps -- A pair form by the minimal element of
                                       -- a set and the original set without
                                       -- it.
        (dRMS,i') = deltaRuleMiniStep v p pps acum i

        
deltaRuleMiniStep :: PolF2 -> PolF2 -> S.Set PolF2 -> S.Set PolF2 -> Integer ->
                     (S.Set PolF2,Integer)
deltaRuleMiniStep v p ps acum counter | S.null ps = (acum,counter)
                                      | dR == 0   = (set0,counter)
                                      | otherwise = deltaRuleMiniStep v p ps'
                                                    (S.insert dR acum) (counter+1)
  where (p',ps') = S.deleteFindMin ps
        dR       = deltaRule v p p'
-------------------------------------------------------------------------------
                                   
-- | __(tool (ps,vvs))__ is verified if the original set of formulas which
-- polynomials from /ps/ came was satisfiable. Otherwise, the function will
-- return False if that set of formulas was unsatisfiable. Note that /vvs/ is
-- the set of variables which occurs in any polynomial from /ps/. For example,
--

-- >>> tool (S.fromList[1],S.empty)
-- True
-- >>> tool (S.fromList[x1,x1+1],S.fromList[x1])
-- False

-- We should think if there exists any way to use the lazy power in the search
-- of zeros.

toolCount :: Integer -> (S.Set (PolF2), S.Set (PolF2)) -> (Bool,Integer)
toolCount i (ps,vvs) | S.null vvs = (S.notMember 0 ps,i)
                     | ps == set0 = (False,i)
                     | otherwise  = toolCount i' (toolNext,vs)
          where (v,vs)            = S.deleteFindMin vvs
                (toolNext,i')     = next v ps i

next v ps i = deltaRule1Step v ps1 ps2 i
  where (ps1,ps2) = S.partition (\p -> mdivides lmv (lm p)) ps
        lmv       = lm v
        --(ps1,ps2) = S.split v ps
        --aux (a,False,b) = (a,b)
        --aux (a,_,b)     = (S.insert v a,b)

toolLC :: Integer -> (S.Set (PolF2),[PolF2]) -> (Bool,Integer)
toolLC i (ps,[])                   = (S.notMember 0 ps,i)
toolLC i (ps,v:vs) | S.member 0 ps = (False,i)
                   | otherwise     = toolLC i' (nL, vs)
                where (nL,i') = nextL v ps i

nextL v ps i = deltaRule1Step v ps1 ps2 i
  where (ps1,ps2) = S.partition (\p -> mdivides lmv (lm p)) ps
        lmv       = lm v

-------------------------------------------------------------------------------

alreves (a,b) = (a, reverse b)

estaV :: PolF2 -> PolF2 -> Int
estaV v p | elem v (vars p) = 1
          | otherwise       = 0

counting :: (S.Set(PolF2),[PolF2]) -> [(PolF2,Int)] -> [(PolF2,Int)]
counting (ps,[]) cs     = cs
counting (ps,(v:vs)) cs = counting (ps,vs) ([(v,(foldl (\acc x -> (estaV v x)+acc) 0 (S.toList ps)))] ++ cs)
-------------------------------------------------------------------------------

-- nextRec 0 (ps,vvs) = ps
-- nextRec n (ps,vvs) | S.null vvs = ps
--                    | otherwise = nextRec (n-1) (next v ps,vs)
--   where (v,vs) = S.deleteFindMin vvs

set0 :: S.Set (PolF2)
set0 = S.fromList [0]

p1,p7,p9,p10 :: PolF2
p1 = var "p1"
p7 = var "p7"
p9 = var "p9"
p10 = var "p10"

pa1,pa2 :: PolF2
pa1 = p1*p10+p1+1
pa2 = (p1*p7*p9+p1*p7+1)

