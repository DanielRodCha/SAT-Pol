module F2Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Math.CommutativeAlgebra.Polynomial
import Math.Core.Field (F2)
import Math.Algebras.VectorSpace 

import F2
import Derivative

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Polinomial properties:" $ do
    context "Addition properties:" ( do
      it "Conmutative" $ property prop_sum_comm
      it "Associative" $ property prop_sum_assoc
      it "Neutral element" $ property prop_sum_neutral
      it "Symmetric" $ property prop_sum_sim
                                      )
    context "Product properties:" ( do
      it "Conmutative" $ property prop_prod_comm
      it "Associative" $ property prop_prod_assoc
      it "Neutral element" $ property prop_prod_neutral
      it "Distributive" $ property prop_distrib
                                        )

-- | Following properties are verified by the polinomial field defined in F2.

prop_sum_comm :: PolF2 -> PolF2 -> Bool
prop_sum_comm p q = p+q == q+p


prop_sum_assoc :: PolF2 -> PolF2 -> PolF2 -> Bool
prop_sum_assoc p q r = p+(q+r) == (p+q)+r


prop_sum_neutral :: PolF2 -> Bool
prop_sum_neutral p = (p + 0 == p) && (0 + p == p)


prop_sum_sim :: PolF2 -> Bool
prop_sum_sim p = p+p == 0


prop_prod_comm :: PolF2 -> PolF2 -> Bool
prop_prod_comm p q = p*q == q*p


prop_prod_assoc :: PolF2 -> PolF2 -> PolF2 -> Bool
prop_prod_assoc p q r = p*(q*r) == (p*q)*r


prop_prod_neutral :: PolF2 -> Bool
prop_prod_neutral p = (p * 1 == p) && (1 * p == p)


prop_distrib :: PolF2 -> PolF2 -> PolF2 -> Bool
prop_distrib p q r = p*(q+r) == (p*q)+(p*r)