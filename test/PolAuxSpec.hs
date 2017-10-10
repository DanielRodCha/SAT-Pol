module PolAuxSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Math.CommutativeAlgebra.Polynomial
import Math.Core.Field (F2)
import Math.Algebras.VectorSpace 

import PolAux

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Propiedades de los polinomios:" $ do
    context "Propiedades de la suma:" ( do
      it "Conmutativa" $ property prop_suma_conmutativa
      it "Asociativa" $ property prop_suma_asociativa
      it "Elem. Neutro" $ property prop_suma_neutro
      it "Simétrica" $ property prop_suma_simetrico
                                      )
    context "Propiedades del producto:" ( do
      it "Conmutativa" $ property prop_prod_conmutativa
      it "Distributiva" $ property prop_distributiva
                                        )
    context "Propiedades del grupo cociente" (do
      it "expTo1 bien definido" $ property prop_expTo1
                                             )
    context "Propiedades de la derivada:" (do
      it "Derivada de la derivada" $ property prop_deriv_deriv
      it "Suma de derivadas" $ property prop_deriv_suma)

-- | Propiedades de la suma:

prop_suma_conmutativa :: PolF2 -> PolF2 -> Bool
prop_suma_conmutativa p q = p+q == q+p

prop_suma_asociativa :: PolF2 -> PolF2 -> PolF2 -> Bool
prop_suma_asociativa p q r = p+(q+r) == (p+q)+r

prop_suma_neutro :: PolF2 -> Bool
prop_suma_neutro p = p + 0 == p

prop_suma_simetrico :: PolF2 -> Bool
prop_suma_simetrico p = p+p == 0

-- | Propiedades del producto:

prop_prod_conmutativa :: PolF2 -> PolF2 -> Bool
prop_prod_conmutativa p q = p*q == q*p

prop_distributiva :: PolF2 -> PolF2 -> PolF2 -> Bool
prop_distributiva p q r = p*(q+r) == (p*q)+(p*r)

-- | Propiedades del grupo cociente:
quotient :: [PolF2] -> [PolF2]
quotient xs = [x^2 + x|x<-xs]

prop_expTo1 :: PolF2 -> Bool
prop_expTo1 p = expTo1 p == p %% (quotient . vars) p

-- | Propiedades de la derivada:

prop_deriv_deriv :: PolF2 -> VarF2 -> Bool
prop_deriv_deriv p v = deriv (deriv p x) x == 0
                       where x = unbox v

prop_deriv_suma :: PolF2 -> PolF2 -> VarF2 -> Bool
prop_deriv_suma p q v = deriv (p+q) x == (deriv p x) + (deriv q x)
                       where x = unbox v

prop_deriv_prod :: PolF2 -> PolF2 -> VarF2 -> Bool
prop_deriv_prod p q v = deriv (p*q) x == (deriv p x)*q + p*(deriv q x)
                       where x = unbox v
