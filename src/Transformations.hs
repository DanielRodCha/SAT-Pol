module Transformations
    ( tr
    , phi
    , theta
    , proyection
    , ideal) where

import Logic
import Haskell4Maths (Vect(..)
                     , var
                     , vars
                     , mindices
                     , eval
                     , linear
                     , (%%))
import F2 (PolF2)

import Test.QuickCheck (quickCheck
                       , maxSize
                       , quickCheckWith
                       , stdArgs)

-- | For example,
--
-- >>> let [p1,p2] = [Atom "p1",Atom "p2"]
-- >>> tr p1
-- x1
-- >>> tr (p1 ∧ p2)
-- x1x2
-- >>> tr (p ∧ (q ∨ r))
-- qrx+qx+rx
tr :: FProp -> PolF2
tr T               = 1
tr F               = 0
tr (Atom ('p':xs)) = var ('x':xs)
tr (Atom xs)       = var xs
tr (Neg a)         = 1 + tr a
tr (Conj a b)      = tr a * tr b
tr (Disj a b)      = a' + b' + a' * b'
                    where a' = tr a
                          b' = tr b
tr (Impl a b)      = 1 + a' + a' * tr b
                    where a' = tr a
tr (Equi a b)      = 1 + tr a + tr b


-- | For example,
--
-- >>> let [x1,x2] = [var "x1", var "x2"] :: [PolF2]
-- >>> theta 0
-- ⊥
-- >>> theta (x1*x2)
-- (p1 ∧ p2)
-- >>> theta (x1 + x2 +1)
-- ¬(p1 ↔ ¬(p2 ↔ ⊤))

theta :: PolF2 -> FProp
theta 0          = F
theta 1          = T
theta (V [m])    = (theta' . mindices . fst) m
theta (V (x:xs)) = no (((theta' . mindices . fst) x) ↔ (theta (V xs)))

theta' :: [(String, t)] -> FProp
theta' []               = T
theta' [(('x':v),i)]    = Atom ('p':v) 
theta' ((('x':v),i):vs) = Conj (Atom ('p':v)) (theta' vs)
theta' [(v,i)]          = Atom v 
theta' ((v,i):vs)       = Conj (Atom v) (theta' vs)

-- | (__phi p__) is the representative with lowest degree of the polynomial p
-- in the quotient group F2[x_1,...,x_N]/(x_1+x_1^2,...,x_N+x_N^2). The main
-- idea is to replace every ocurrence of x_i^M with x_i thus we obtain an
-- identical polynomial without exponents greater than 1.
--
-- For example,
-- >>> [x1,x2] = [var "x1", var "x2"] :: [PolF2]
-- >>> phi (1+x1+x1^2*x2) 
-- x1x2+x1+1
phi :: PolF2  -> PolF2
phi = linear (\m -> product [ var x | (x,i) <- mindices m])

-- | For example,
--
-- >>> [x1,x2] = [var "x1", var "x2"] :: [PolF2]
-- >>> ideal (1+x1+x1^2*x2)
-- [x1^2+x1,x2^2+x2]
ideal :: PolF2 -> [PolF2]
ideal p = [v+v^2| v<-vars p]



-- | For example,
-- >>> [p1,p2] = [Atom "p1",Atom "p2"]
-- >>> proyection p1
-- x1
-- >>> tr (p1 → p1 ∧ p2)
-- x1^2x2+x1+1
-- >>> proyection (p1 → p1 ∧ p2)
-- x1x2+x1+1
proyection :: FProp -> PolF2
proyection = phi . tr

