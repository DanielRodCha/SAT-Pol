module DerivativeSpec (main, spec) where

import Data.List (union)
import Test.Hspec
import Test.QuickCheck

import Math.CommutativeAlgebra.Polynomial
import Math.Core.Field (F2)
import Math.Algebras.VectorSpace 

import F2
import Logic
import Transformations
import Derivative

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Polinomial properties:" $ do
    context "Derivative properties:" (do
      it "Derivative of the derivative" $ property prop_deriv_deriv
      it "Addition" $ property prop_deriv_sum
      it "Semantic equivalence of derivative" $ property prop_deriv
      )


-- | Following properties are verified by the polinomial derivative
-- over field PolF2 defined in Derivative module.

prop_deriv_deriv :: PolF2 -> VarF2 -> Bool
prop_deriv_deriv p v = derivPol (derivPol p x) x == 0
                       where x = unbox v

prop_deriv_sum :: PolF2 -> PolF2 -> VarF2 -> Bool
prop_deriv_sum p q v = derivPol (p+q) x == (derivPol p x) + (derivPol q x)
                       where x = unbox v


prop_deriv :: FProp -> Int -> Bool
prop_deriv f n = equivalent (theta (derivPol pol v))
                                 (no (Equi (substitute f varP (no p)) f))
  where pol           = proyection f
        vs            = union (vars pol) [((var "x") :: PolF2)]
        v             = vs !! (n `mod` (length vs))
        p             = theta v
        aux (Atom xs) = xs
        varP          = aux p
