module Rule where

import Logic
import Haskell4Maths ( var
                     , vars)
import F2 (PolF2)
import Transformations ( phi
                        , theta
                        , proyection)
import Derivative (derivPol)

import Data.List (union)
import Test.QuickCheck (quickCheck)
import qualified Data.Set as S

-- | (independenceRule v p1 p2) is the polynomial obtained from apply the independence
-- rule described in the paper [?] between polynomials /p1/ /p2/ respect the
-- variable /v/. It's important to note that as /v/ is the variable from wich we
-- derive, it won't occurs in the output polynomial. For example,
-- 
-- >>> [x1,x2,x3,x4] = (map var ["x1","x2","x3","x4"]) :: [PolF2]
-- >>> independenceRule x1 1 1
-- 1
-- >>> independenceRule x1 1 0
-- 0
-- >>> independenceRule x1 x1 x1
-- 1
-- >>> independenceRule x1 x1 x1*x2
-- x2
-- >>> independenceRule x1 (x1*x3) (x1*x2)
-- x2x3
-- >>> independenceRule x1 (1+x1*x3) (x1*x2)
-- x2x3+x2
independenceRule :: PolF2 -> PolF2 -> PolF2 -> PolF2
independenceRule x a1 a2 = aux + a1a2 + aux2
  where da1       = derivPol a1 x
        da2       = derivPol a2 x
        a1a2      = phi $ a1*a2
        a1da2     = phi $ a1*da2
        a2da1     = phi $ a2*da1
        da1da2    = phi $ da1*da2
        aux       = phi $ a1da2 + a2da1 + da1da2
        aux2      = phi $ a1a2*aux

-- | Is the independence rule defined above but applied to
-- propositional formulas.
independenceRuleForm :: VarProp -> FProp -> FProp -> FProp
independenceRuleForm p f1 f2 = theta $ independenceRule x p1 p2
  where x  = proyection (Atom p)
        p1 = proyection f1
        p2 = proyection f2