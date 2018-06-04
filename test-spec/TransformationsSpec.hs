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
      modifyMaxSize (const 50) $ it "(theta (projection f)) equivalent to (f)" $ property prop_theta_projection
      it "(projection (theta p)) equal to (phi p)" $ property prop_theta_projection
      it "phi" $ property prop_phi
      


prop_theta_tr :: FProp -> Bool
prop_theta_tr f = equivalent (theta (tr f)) f

prop_theta_projection :: FProp -> Bool
prop_theta_projection f = equivalent (theta (projection f)) f


prop_projection_theta :: PolF2 -> Bool
prop_projection_theta p = phi p == (projection . theta) p

prop_phi :: PolF2 -> Bool
prop_phi p = phi p == p %% (ideal p)
