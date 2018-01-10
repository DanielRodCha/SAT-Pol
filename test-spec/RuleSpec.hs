module RuleSpec (main, spec) where

import Data.List (union)
import Test.Hspec
import Test.QuickCheck

--import Math.CommutativeAlgebra.Polynomial
--import Math.Core.Field (F2)
--import Math.Algebras.VectorSpace 

import Haskell4Maths
import F2
import Rule
import Derivative
import Transformations (phi)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Tool properties" $ do
    context "Properties of the independence rule" ( do
      it "Well defined" $ property prop_independenceRule_correct
      it "Symmetric" $ property prop_independenceRule_symmetric
      it "Commutative" $ property prop_independenceRule_commutative
      it "The independence rule is a forgetting operator" $ property prop_independenceRule_forget
                                      )

prop_independenceRule_symmetric :: PolF2 -> PolF2 -> Int -> Bool
prop_independenceRule_symmetric a1 a2 n = independenceRule x a1' a2' ==
                                    independenceRule x a2' a1'
  where a1' = phi a1
        a2' = phi a2
        xs  = union (vars a1') (vars a2')
        xss = if (null xs) then [((var "x") :: PolF2)]
                           else xs
        x   = xss !! (n `mod` (length xss))


independenceRule' :: PolF2 -> PolF2 -> PolF2 -> PolF2
independenceRule' p a1 a2 = 1+((1+a1*a2)*(1+a1*da2+a2*da1+da1*da2))
  where da1       = derivPol a1 p
        da2       = derivPol a2 p

prop_independenceRule_correct :: VarF2 -> PolF2 -> PolF2 -> Bool
prop_independenceRule_correct v p q = independenceRule x p q == phi (independenceRule' x p q)
  where x = unbox v

prop_independenceRule_commutative :: VarF2 -> PolF2 -> PolF2 -> Bool
prop_independenceRule_commutative v p q = independenceRule x p q == independenceRule x q p
  where x = unbox v

prop_independenceRule_forget :: VarF2 -> PolF2 -> PolF2 -> Bool
prop_independenceRule_forget v p' q' = notElem x $ vars $ independenceRule x p q
  where x = unbox v
        p = phi p'
        q = phi q'
