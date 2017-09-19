module LogicAux where

--import Data.Logic.Propositional(variables)
import Data.List (group, sort)

import Math.CommutativeAlgebra.Polynomial
import Math.Core.Field (F2)
import Math.Algebras.Structures
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Core.Utils (toSet)

newtype Var = Var String
  deriving (Eq, Ord)

instance Show Var where
  show (Var v) = v

data Expr = Variable      Var
          | Negation      Expr
          | Conjunction   Expr Expr
          | Disjunction   Expr Expr
          | Conditional   Expr Expr
          | Biconditional Expr Expr
          deriving Eq

instance Show Expr where
  show (Variable      name)      = show name
  show (Negation      expr)      = '¬' : show expr
  show (Conjunction   exp1 exp2) = showBC "∧" exp1 exp2
  show (Disjunction   exp1 exp2) = showBC "∨" exp1 exp2
  show (Conditional   exp1 exp2) = showBC "→" exp1 exp2
  show (Biconditional exp1 exp2) = showBC "↔" exp1 exp2

-- | Represents expressions using only ASCII characters (the 'show' function
-- pretty-prints expressions using logical symbols only present in extended
-- character sets).
showAscii :: Expr -> String
showAscii (Variable      name)      = show name
showAscii (Negation      expr)      = '~' : showAscii expr
showAscii (Conjunction   exp1 exp2) = showBCA "&"   exp1 exp2
showAscii (Disjunction   exp1 exp2) = showBCA "|"   exp1 exp2
showAscii (Conditional   exp1 exp2) = showBCA "->"  exp1 exp2
showAscii (Biconditional exp1 exp2) = showBCA "<->" exp1 exp2

showBinaryConnective :: (Expr -> String) -> String -> Expr -> Expr -> String
showBinaryConnective show_ symbol exp1 exp2 =
  '(' : show_ exp1 ++ " " ++ symbol ++ " " ++ show_ exp2 ++ ")"

showBC :: String -> Expr -> Expr -> String
showBC = showBinaryConnective show

showBCA :: String -> Expr -> Expr -> String
showBCA = showBinaryConnective showAscii

variables :: Expr -> [Var]
variables expr =
  let vars_ (Variable      v)     vs = v : vs
      vars_ (Negation      e)     vs = vars_ e vs
      vars_ (Conjunction   e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
      vars_ (Disjunction   e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
      vars_ (Conditional   e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
      vars_ (Biconditional e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
  in  map head . group . sort $ vars_ expr []

declaraVar :: (Num k, MonomialConstructor m) => Expr -> [Vect k (m Var)]
declaraVar form = map var (variables form)



