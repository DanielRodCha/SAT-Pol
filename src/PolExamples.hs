-- |This module contains several examples in order to verify the good
-- functioning of the other modules.
module PolExamples where

import Data.List (nub,iterate,partition,foldl')
import qualified Data.Set as S

import Math.CommutativeAlgebra.Polynomial
import Math.Core.Field (F2)
import Math.Algebras.Structures
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Core.Utils (toSet)

import System.Environment

import qualified Data.Set as S

-------------------------------------------------------------------------

[x1,x2,x3,x4,x11] =
  map var ["x1","x2","x3","x4","x11"] :: [LexPoly F2 String]

examples :: [LexPoly F2 String]
examples = [1,x1*x2+x1+x2,x1*x2+x1+1,x1*x2+1,x1*x2+x2+1]

exampleSet :: S.Set (LexPoly F2 String)
exampleSet = S.fromList [1,x1*x2+x1+x2,x1*x2+x1+1,x1*x2+1,x1*x2+x2+1]

-- monx :: Lex String
-- --polx :: Vect F2 (Lex String)
-- monx = Lex (M 1 [("x1",1)])

exampleMonomial1 = (Lex (M 1 []))
exampleMonomial2 = (Lex (M 1 [("x1",1)]))
exampleMonomial3 = (Lex (M 1 [("x1",1),("x2",1)]))

--ejMonomio = Lex (M 1 [("x1",1),("y2",2)])
-- ejMonomio2 = Lex (M 1 [("x1",2),("y2",2)])

-- ej1 :: LexPoly F2 String
-- --ej1 :: Vect F2 (Lex String)
-- ej1 = 3*x1*y2+x1^2+y2+z0

ej2 :: Vect F2 (Lex String)
-- --ej2 :: LexPoly F2 String
ej2 = x4*x2^4+x1+x1^34*x2^12

ej1 :: LexPoly F2 String
ej1 = x1*x2+x2*x3+x2+x4*x11+x11*x1*x2

-- ej3 :: LexPoly F2 String
ej3 :: LexPoly F2 String
ej3 = ej1^1001

-- | Ejemplo de grado
-- 
-- >>> deg ej3
-- 3003
