-- | This module aims to provide an efficient tool to solve the SAT
-- problem. Both the algorithm and the implementation have been carried out by
-- the researchers from the department of Computer Science and Artificial
-- Intelligence at the University of Seville.

module Tool
    ( tool
    , toolN
    , toolL
    , deltaRule
    , deltaRule1Step
    , next
    ) where

import Data.List (nub,iterate,partition, foldl', union)

import PolAux
import Examples

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
-- >>> deltaRule1Step x1 (S.fromList [x1]) (S.fromList [1]) 
-- fromList [1]
-- >>> deltaRule1Step x1 (S.fromList [x1,x1*x2,x1*x3]) (S.empty) 
-- fromList [x2x3,x2,x3,1]

deltaRule1Step :: PolF2 -> S.Set (PolF2) ->
                  S.Set (PolF2) -> S.Set (PolF2)
deltaRule1Step v pps acum | S.null pps = acum
                          | otherwise  = deltaRule1Step v ps miniStep
  where (p,ps)   = S.deleteFindMin pps -- A pair form by the minimal element of
                                       -- a set and the original set without
                                       -- it. 
        miniStep = S.foldr (\p' acc -> S.insert (deltaRule v p p') acc) acum pps 
        

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

tool :: (S.Set (PolF2), S.Set (PolF2)) -> Bool
tool (ps,vvs) | S.null vvs     = S.notMember 0 ps
              | S.member 0 ps  = False
              | otherwise      = tool (ps',vs)
          where (v,vs)    = S.deleteFindMin vvs
                --(ps1,ps2) = S.partition (\p -> aux (vars p)) ps -- time leak?
                (ps1,ps2) = S.partition (\p -> mdivides (lm v) (lm p)) ps
                ps'       = deltaRule1Step v ps1 ps2
                --aux x   | null x    = False
                --        | otherwise = v == head x

toolL :: (S.Set (PolF2),[PolF2]) -> Bool
toolL (ps,[]) = S.notMember 0 ps
toolL (ps,v:vs) | S.member 0 ps     = False
                | otherwise      = toolL (nextL v ps, vs)

nextL v ps = deltaRule1Step v ps1 ps2
  where (ps1,ps2) = split' v ps
-------------------------------------------------------------------------------

toolN :: S.Set (PolF2) -> Bool
toolN ps | S.member 0 ps          = False
         | S.findMin ps == 1 = True
         | otherwise      = toolN $ next ps
          -- where v         = fst $ head $ mindices $ lm $ S.findMin ps
          --       aux a     = (null a || v == (fst . head) a)
          --       (ps1,ps2) = S.partition
          --         (\p -> (aux . mindices . lm) p) ps
          --       ps'       = deltaRule1Step (var v) ps1 ps2

next ps = deltaRule1Step v ps1 ps2
  where  v         = var $ fst $ head $ mindices $ lm $ S.findMin ps
         (ps1,ps2) = split' v ps
         --(ps1,ps2) = S.partition (\p -> ((v == ) . fst . head' . mindices .
         -- lm) p) ps

nextN 0 ps = ps
nextN n ps = nextN (n-1) (next ps)

split' v ps = aux $ S.splitMember v ps
  where aux (a,False,b) = (a,b)
        aux (a,_,b)     = (S.insert v a,b)

head' []     = ("error",1)
head' (x:xs) = x
