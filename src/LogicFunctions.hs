module LogicFunctions
    ( form2Pol
    , pol2Form
    , example)
  where

import Math.CommutativeAlgebra.Polynomial
import Math.Core.Field (F2)
import Math.Algebras.Structures
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Core.Utils (toSet)

import LogicAux
import PolAux

import qualified Data.Set as S

form2Pol :: Expr -> Vect F2 (Lex String)
form2Pol (Variable      (Var a))   = (var a)
form2Pol (Negation      expr)      = 1 + form2Pol expr
form2Pol (Conjunction   exp1 exp2) = (form2Pol exp1) * (form2Pol exp2)
form2Pol (Disjunction   exp1 exp2) =
  (form2Pol exp1) + (form2Pol exp2) + (form2Pol exp1) * (form2Pol exp2)
form2Pol (Conditional   exp1 exp2) =
  1 + (form2Pol exp1) + (form2Pol exp1) * (form2Pol exp2)
form2Pol (Biconditional exp1 exp2) = 1 + (form2Pol exp1) + (form2Pol exp2)

conjunc :: [(String,t)] -> Expr
conjunc [(x,_)] = Variable (Var x)
conjunc ((x,_):xs) = Conjunction (Variable (Var x)) (conjunc xs)

pol2Form :: MonomialConstructor m => Vect k (m String) -> Expr
pol2Form (V [(x,_)])    = conjunc (mindices x)
pol2Form (V ((x,_):xs)) = Disjunction (conjunc (mindices x)) (pol2Form (V xs))

r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11 :: Expr
r1 = Conditional (Variable (Var "p1")) (Variable (Var "p9"))
r2 = Conditional (Variable (Var "p1")) (Variable (Var "p10"))
r3 = Conditional (Negation (Variable (Var "p2"))) (Variable (Var "p9"))
r4 = Conditional (Negation (Variable (Var "p2"))) (Variable (Var "p10"))
r5 = Conditional (Conjunction (Variable (Var "p1")) (Variable (Var "p7"))) (Variable (Var "p9"))
r6 = Conditional (Variable (Var "p3")) (Variable (Var "p7"))
r7 = Conditional (Variable (Var "p3")) (Variable (Var "p10"))
r8 = Conditional (Variable (Var "p4")) (Variable (Var "p11"))
r9 = Conditional (Variable (Var "p5")) (Variable (Var "p8"))
r10 = Conditional (Variable (Var "p6")) (Variable (Var "p9"))
r11 = Negation (Variable (Var "p2"))

example = S.fromList (map form2Pol [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11])
