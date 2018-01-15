module Preprocessing where


import Haskell4Maths (var
                     , zerov
                     , vars)
import F2
import Transformations ( phi, proyection)
import Subsumption
--import LogicParser
import Analizador (parseFProp)

import Data.List (foldl')
import Data.Char
import qualified Data.Set as S

-- | (__ literal2Pol l __)is the pair /(p,v)/, where /p/ is the polynomial and /v/ is
-- the variable that corresponds (if it does) to the literal /lit/ (in DIMACS
-- format). For example,
--
-- >>> literal2Pol "0"
-- (0,0)
-- >>> literal2Pol "1"
-- (x1,x1)
-- >>> literal2Pol "-1"
-- (x1+1,x1)
literal2Pol :: String -> (PolF2,PolF2)
literal2Pol "0"       = (zerov,zerov)
literal2Pol ('-':lit) = (1 + x,x)
                   where x = var ('x':lit)
literal2Pol lit       = (x,x)
                   where x = var ('x':lit)

-- | __(clause2pol cs)__ is a pair /(p,vs)/, where /p/ is the polynomial that
-- corresponds to the clause /cs/ (which is written in DIMACS format) and /vs/ is
-- the set of its variables. For example,
--
-- >>> clause2Pol ["1"]
-- (x1,fromList [x1])
-- >>> clause2Pol ["1","-2"]
-- (x1x2+x2+1,fromList [x1,x2])
clause2Pol :: [String] -> (PolF2, S.Set (PolF2))
clause2Pol (c:cs) | c == "c" || c == "p" = (1, S.empty)
clause2Pol cs = aux $ foldl' (\acc x -> disj (literal2Pol x) acc)
                                     (zerov,S.empty) cs
  where aux (a,b) = (phi a,b)
        disj (x,v) (y,vs) | v == zerov = (y,vs)
                          | otherwise   = (x + y + x*y, S.insert v vs)

-- | __(dimacs2pols f)__ is the pair (/ps/,/vs/) where ps is the set of polynomials
-- wich corresponds to the formula in DIMACS format writed in the file /f/ and
-- /vs/ is the list of variables wich occurs in any polynomial. For example,
--
-- >>> dimacs2Pols "exDIMACS/easy/example1.txt"
-- (fromList [x1x2+x1+x2,1],[x1,x2])
-- >>> dimacs2Pols "exDIMACS/easy/example4.txt"
-- (fromList [x1x2+x1+x2,x1x2+x1+1,x1x2+x2+1,x1x2+1,1],[x1,x2])
dimacs2Pols f = do
  s0 <- readFile f
  return $
    aux1 $ (foldr (\x acc -> (aux2 ((clause2Pol . words) x) acc))
             (S.empty,S.empty)) $ lines $ s0
     where aux1 (a,b) = (a,S.toList b)
           aux2 (a,b) (acc',vs) = (S.insert a acc, S.union vs b)
--           aux2 (a,b) (acc',vs) = (S.insert a (removeDivisors a acc'), S.union vs b)

-- | __(formulas2Pols f)__ is the pair (/ps/,/vs/) where ps is the set of polynomials
-- wich corresponds to the formula in FORMULAS format writed in the file /f/ and
-- /vs/ is the list of variables wich occurs in any polynomial. For example,
formulas2Pols f = do
  s0 <- readFile f
  return $
    aux1 $ (foldr (\x acc -> (aux2 (aux3 x) acc))
             (S.empty,S.empty)) $ lines $ s0
     where aux1 (a,b) = (a,S.toList b)
           aux2 (a,b) (acc',vs) = (S.insert a acc, S.union vs b)
--           aux2 (a,b) (acc',vs) = (S.insert a (removeDivisors a acc'), S.union vs b)
           aux3 = aux4 . proyection . parseFProp
           aux4 x = (x,S.fromList (vars x))