-- | This module aims to provide an efficient tool to solve the SAT
-- problem. Both the algorithm and the implementation have been carried out by
-- the researchers from the department of Computer Science and Artificial
-- Intelligence at the University of Seville.

module ToolS
    ( toolS
    , toolSP'
    ) where

import Data.List(nub,iterate,partition, foldl', union)

import PolAux
import Examples

import Math.CommutativeAlgebra.Polynomial
import Math.Core.Field (F2)
import Math.Algebras.Structures
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Core.Utils (toSet)

import qualified Data.Set as S

--- ------------------------------------------------------------------------------

-- -- |(pickAux s) implements the axiom of choice (AC) but not randomly. In fact, as
-- -- the data type is represented as a tree, we will choose its root.

-- --pickAux :: S.Set a -> Maybe a
-- pickAux Tip = Nothing
-- pickAux (Bin _ x _ _) = Just x

-- -- |(pick s) select an element from the set s.

-- --pick :: S.Set a -> a
-- pick s
--   | Just r <- pickAux s = r
--   | otherwise = error "pick: The empty set has no eligible element"


-------------------------------------------------------------------------------

-- | varsSet returns the set of variables which occurs in a polynomial. For
-- example:
-- 
-- >>> varsSet (x2*x3+x1)
-- fromList [x1,x2,x3]
-- >>> varsSet (x1)
-- fromList [x1]

varsSet :: (Ord (m v)
           , Num k
           , Ord k
           , MonomialConstructor m) =>
           Vect k (m v) -> S.Set (Vect k (m v))
varsSet = S.fromDistinctAscList . vars

-- |varsList xs returns the set of variables which occurs in any polynomial of
-- the set xs. For example:

varsList :: (Foldable t
            , Num k
            , MonomialConstructor m
            , Ord (m v)
            , Ord k) =>
            t (Vect k (m v)) -> S.Set (Vect k (m v))
varsList = foldr (\vs acc -> S.union acc (varsSet vs)) S.empty
-------------------------------------------------------------------------------

-- | The function (deltaRule p x y) performs the independence rule described in
-- the paper [?]. It's important to note that p is the variable from wich we
-- derive and the one we would drop. For example:
-- 
-- >>> deltaRule x1 1 1
-- 1
-- >>> deltaRule x1 1 0
-- 0
-- >>> deltaRule x1 x1 x1
-- 1

deltaRule :: Vect F2 (Lex String) -> Vect F2 (Lex String) ->
             Vect F2 (Lex String) -> Vect F2 (Lex String)
deltaRule p a1 a2 = cleanExp (aux + a1a2 + aux2)
  --clean (a1da2 + a2da1 + da1da2 + a1a2 + a1a2a1da2 + a1a2a2da1 + a1a2da1da2)
  where da1        = deriv a1 p
        da2        = deriv a2 p
        a1a2       = cleanExp (a1*a2)
        a1da2      = cleanExp a1*da2
        a2da1      = cleanExp a2*da1
        da1da2     = cleanExp da1*da2
        aux        = cleanExp (a1da2 + a2da1 + da1da2)
        aux2       = cleanExp (a1a2*aux)
        --a1a2a1da2  = clean (a2*a1da2)
        --a1a2a2da1  = clean (a1*a2da1)
        --a1a2da1da2 = clean (a1a2*da1da2)
        

-------------------------------------------------------------------------------

-- | deltaRule1Step apply deltaRule from p between every polynomial in the
-- first list and store the results in the accumulator (second list). For
-- example:
-- 
-- >>> deltaRule1Step x1 (S.fromList [x1]) (S.fromList [1]) 
-- fromList [1]
-- >>> deltaRule1Step x1 (S.fromList [x1,x1*x2,x1*x3]) (S.empty) 
-- fromList [x2x3,x2,x3,1]

deltaRule1Step :: Vect F2 (Lex String) -> S.Set (Vect F2 (Lex String)) ->
                  S.Set (Vect F2 (Lex String)) -> S.Set (Vect F2 (Lex String))
deltaRule1Step v pps acum | S.null pps = acum
                          | otherwise  = deltaRule1Step v ps miniStep
  where (p,ps)   = S.deleteFindMin pps -- A pair form by the minimal element of
                                       -- a set and the original set without
                                       -- it. 
        miniStep = S.foldr (\p' acc -> S.insert (deltaRule v p p') acc) acum pps 
        

-------------------------------------------------------------------------------
                                   
-- |toolAux check if in any step of the algorithm a zero is obtained. In this
-- case, the original set of formulas was unsatisfiable and the tool answer
-- would be "False". Otherwise, the set of polynomials is divided in two subsets,
-- one contains those polynomials in which occurs the variable p, while the
-- other stores the rest.

-- We should think if there exists any way to use the lazy power in the search
-- of zeros.


toolAux :: S.Set (Vect F2 (Lex String)) -> S.Set (Vect F2 (Lex String)) -> Bool
toolAux vvs ps | S.null vvs        = S.notMember 0 ps
               | S.member 0 ps  = False
               | otherwise      = toolAux vs ps'
          where (v,vs)    = S.deleteFindMin vvs
                --(ps1,ps2) = S.partition (\p -> aux (vars p)) ps -- time leak?
                (ps1,ps2) = S.partition (\p -> mdivides (lm v) (lm p)) ps
                ps'       = deltaRule1Step v ps1 ps2
                --aux x   | null x    = False
                --        | otherwise = v == head x
-------------------------------------------------------------------------------

-- |tool decides if the set of formulas that produced the set of polynomials
-- were satisfiables. The function input is a list of polynomials because the
-- transformation from formula to polynomial is handled by ReadingF.hs module.

toolS :: S.Set (Vect F2 (Lex String)) -> Bool
toolS xs = toolAux (varsList xs) xs

-------------------------------------------------------------------------------


toolSP' :: (S.Set (Vect F2 (Lex String)), S.Set (Vect F2 (Lex String))) -> Bool
toolSP' (xs,vs) = toolAux vs xs

-------------------------------------------------------------------------------
