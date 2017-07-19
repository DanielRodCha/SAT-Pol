{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module PolAux
    ( cleanExp,
      deriv,      
    ) where

import Math.CommutativeAlgebra.Polynomial (Lex,var,mindices,lm, (%%))
import Math.Core.Field (F2)
import Math.Algebras.VectorSpace (Vect, linear)

import Examples

-------------------------------------------------------------------------------

-- | cleanExp aims to select the representative with leastest degree of a
-- polynomial in the quotient group F2[x_1,...,x_N]/(x_1+x_1^2,...,x_N+x_N^2)
-- . The main idea is to replace every ocurrence of x_i^M with x_i thus we
-- obtain an identical polynomial without exponents greater than 1.
--
-- In the library HaskellForMaths exists a function that performs the
-- same (%%) so we can check the results. For example,
--
-- >>> cleanExp (x1^3)
-- x1
-- >>> (x1^3) %% [x1^2+x1]
-- x1
-- >>> cleanExp (x1^3*x2^6+x3^2*x4+x1+1)
-- x1x2+x1+x3x4+1
-- >>> let pol = x1^3*x2^6+x3^2*x4+x1+1
-- >>> let list = [x1^2+x1,x2^2+x2,x3^2+x3,x4^2+x4] 
-- >>> pol %% list
-- x1x2+x1+x3x4+1

cleanExp :: Vect F2 (Lex String)  -> Vect F2 (Lex String)
cleanExp v = linear (\m -> product [ var x | (x,i) <- mindices m]) v

-------------------------------------------------------------------------------

-- | derivMon calculates the derivative of a monomial m with respect to the
-- variable v. It's important to note that only works if it applies to
-- monomials without exponents greater than 1.
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

derivMon :: (Lex String) -> Vect F2 (Lex String) -> Vect F2 (Lex String)
derivMon m v
  | varDif `elem` mIndices =
      product [var x ^ i | (x,i) <- mIndices, x /= fst varDif]
  | otherwise =  0
  where mIndices = mindices m
        varDif = head (mindices (lm v))

-- | deriv calculates the derivative of the polynomial p with respect to the
-- variable v. It's important to note that deriv only works if it applies to
-- polynomials without exponents greater than 1. In practice, deriv will only
-- be used with polynomials that have been previously "cleaned". For example:
--
-- >>> let v = x1
-- >>> deriv v v
-- 1
-- >>> deriv (1+x1+x2+x1*x2) v
-- x2+1
-- >>> deriv (x1*x2+x1+x3*x4+1) v
-- x2+1
deriv :: Vect F2 (Lex String) -> Vect F2 (Lex String) -> Vect F2 (Lex String)
deriv p v = linear (`derivMon` v) p
                       
