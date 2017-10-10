module ToolSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Math.CommutativeAlgebra.Polynomial
import Math.Core.Field (F2)
import Math.Algebras.VectorSpace 

import PolAux
import Tool

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Propiedades de la herramienta:" $ do
    context "Propiedades de la regla delta:" ( do
      it "Bien definida" $ property prop_deltaRule_correcta
      it "Conmutativa" $ property prop_deltaRule_conmutativa
      it "Es regla de eliminación de variables" $ property prop_deltaRule_elim
                                      )

-- | Propiedades de la regla delta:
deltaRule' :: PolF2 -> PolF2 -> PolF2 -> PolF2
deltaRule' p a1 a2 = 1+((1+a1*a2)*(1+a1*da2+a2*da1+da1*da2))
  where da1       = deriv a1 p
        da2       = deriv a2 p

prop_deltaRule_correcta :: VarF2 -> PolF2 -> PolF2 -> Bool
prop_deltaRule_correcta v p q = deltaRule x p q == expTo1 (deltaRule' x p q)
  where x = unbox v

prop_deltaRule_conmutativa :: VarF2 -> PolF2 -> PolF2 -> Bool
prop_deltaRule_conmutativa v p q = deltaRule x p q == deltaRule x q p
  where x = unbox v

prop_deltaRule_elim :: VarF2 -> PolF2 -> PolF2 -> Bool
prop_deltaRule_elim v p' q' = notElem x $ vars $ deltaRule x p q
  where x = unbox v
        p = expTo1 p'
        q = expTo1 q'
