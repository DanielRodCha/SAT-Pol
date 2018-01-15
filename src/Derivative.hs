module Derivative (derivPol) where

import Logic
import Haskell4Maths ( lm
                     , var
                     , vars
                     , linear
                     , mindices
                     , Lex (..)
                     , MonImpl(..))
import F2 (PolF2)
import Transformations (proyection
                        , theta)
       
import Data.List (union)
import Test.QuickCheck (quickCheck)

-- | (__derivMon m v__) is the derivative of the monomial m with respect to the
-- variable v.
--
-- $ It's important to note that only works if it applies to monomials without exponents greater than 1.
-- For example,
--
-- >>> x1 = var "x1" :: PolF2
-- >>> exampleMonomial1 = (Lex (M 1 []))
-- >>> exampleMonomial1
-- 1
-- >>> derivMon exampleMonomial1 x1
-- 0
-- >>> exampleMonomial2 = (Lex (M 1 [("x1",1)]))
-- >>> exampleMonomial2
-- x1
-- >>> derivMon exampleMonomial2 x1
-- 1
-- >>> exampleMonomial3 = (Lex (M 1 [("x1",1),("x2",1)]))
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

-- | (__derivPol p v__) is the derivative of the polynomial p with respect to the
-- variable v. It's important to note that deriv only works if it applies to
-- polynomials without exponents greater than 1. In practice, deriv will only
-- be used with the polynomials that have been previously embeded in the
-- quotient group described above. For example,
--
-- >>> [x1,x2,x3,x4] = (map var ["x1","x2","x3","x4"]) :: [PolF2]
-- >>> derivPol x1 x1
-- 1
-- >>> derivPol (1+x1+x2+x1*x2) x1
-- x2+1
-- >>> derivPol (x1*x2+x1+x3*x4+1) x1
-- x2+1
derivPol :: PolF2 -> PolF2 -> PolF2
derivPol p v = linear (`derivMon` v) p