module TransformationsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (modifyMaxSize)

import Math.CommutativeAlgebra.Polynomial
import Math.Core.Field (F2)
import Math.Algebras.VectorSpace 

import F2
import Logic
import Transformations

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Transformations properties:" $ do
      modifyMaxSize (const 30) $ it "(theta (tr f)) equivalent to (f)" $ property prop_theta_tr
      modifyMaxSize (const 50) $ it "(theta (proyection f)) equivalent to (f)" $ property prop_theta_proyection
      it "(proyection (theta p)) equal to (phi p)" $ property prop_theta_proyection
      it "phi" $ property prop_phi
      


prop_theta_tr :: FProp -> Bool
prop_theta_tr f = equivalent (theta (tr f)) f

prop_theta_proyection :: FProp -> Bool
prop_theta_proyection f = equivalent (theta (proyection f)) f


prop_proyection_theta :: PolF2 -> Bool
prop_proyection_theta p = phi p == (proyection . theta) p

prop_phi :: PolF2 -> Bool
prop_phi p = phi p == p %% (ideal p)
