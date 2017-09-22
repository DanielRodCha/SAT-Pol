{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module PolAux
    ( PolF2
    , expTo1
    , deriv
    , var
    , vars
    , zerov
    , mindices
    , lm
    ) where

import Math.CommutativeAlgebra.Polynomial (Grevlex, Glex, Lex ,var,mindices,lm, (%%), vars)
import Math.Core.Field (F2)
import Math.Algebras.VectorSpace (Vect, linear, zerov)

import PolExamples

-------------------------------------------------------------------------------
-- | The data type PolF2 is the field of polynomial with coefficients in the
-- finite field F2.

type PolF2 = Vect F2 (Lex String)

-------------------------------------------------------------------------------

-- | (__expTo1 p__) is the representative with leastest degree of the polynomial p
-- in the quotient group F2[x_1,...,x_N]/(x_1+x_1^2,...,x_N+x_N^2). The main
-- idea is to replace every ocurrence of x_i^M with x_i thus we obtain an
-- identical polynomial without exponents greater than 1. 
--
-- In the library HaskellForMaths exists a function that performs the
-- same (%%) so we can check the results. For example,
--
-- >>> expTo1 (x1^3)
-- x1
-- >>> (x1^3) %% [x1^2+x1]
-- x1
-- >>> expTo1 (x1^3*x2^6+x3^2*x4+x1+1)
-- x1x2+x1+x3x4+1
-- >>> let pol = x1^3*x2^6+x3^2*x4+x1+1
-- >>> let list = [x1^2+x1,x2^2+x2,x3^2+x3,x4^2+x4] 
-- >>> pol %% list
-- x1x2+x1+x3x4+1

expTo1 :: PolF2  -> PolF2
expTo1 v = linear (\m -> product [ var x | (x,i) <- mindices m]) v

-------------------------------------------------------------------------------

-- | (derivMon m v) is the derivative of the monomial m with respect to the
-- variable v.
--
-- $ It's important to note that only works if it applies to monomials without exponents greater than 1.
--
-- >>> exampleMonomial1
-- 1
-- >>> derivMon exampleMonomial1 x1
-- 0
-- >>> exampleMonomial2
-- x1
-- >>> derivMon exampleMonomial2 x1
-- 1
-- >>> exampleMonomial3
-- x1x2
-- >>> derivMon exampleMonomial3 x1
-- x2

derivMon :: (Lex String) -> PolF2 -> PolF2
derivMon m v
  | varDif `elem` mIndices =
      product [var x ^ i | (x,i) <- mIndices, x /= fst varDif]
  | otherwise =  0
  where mIndices = mindices m
        varDif   = head (mindices (lm v))

-- | (__deriv p v__) is the derivative of the polynomial p with respect to the
-- variable v. It's important to note that deriv only works if it applies to
-- polynomials without exponents greater than 1. In practice, deriv will only
-- be used with the polynomials that have been previously embeded in the
-- quotient group described above. For example,
-- >>> let v = x1
-- >>> deriv v v
-- 1
-- >>> deriv (1+x1+x2+x1*x2) v
-- x2+1
-- >>> deriv (x1*x2+x1+x3*x4+1) v
-- x2+1

deriv :: PolF2 -> PolF2 -> PolF2
deriv p v = linear (`derivMon` v) p
                       
