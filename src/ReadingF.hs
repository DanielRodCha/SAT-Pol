{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module ReadingF where

import Data.List
import Data.Char (isSpace)
import Data.Foldable (sum, product)
import System.Environment

import qualified Data.Set as S

import PolAux (PolF2, expTo1, var, zerov)
import Tool (tool)

-------------------------------------------------------------------------------
-- | __(clause2pol cs)__ is a pair /(p,vs)/, where /p/ is the polynomial that
-- corresponds to the clause /cs/ (which is written in DIMACS format) and /vs/ is
-- the set of its variables.
--
-- >>> clause2pol ["1"]
-- (x1,fromList [x1])
-- >>> clause2pol ["1","-2"]
-- (x1x2+x2+1,fromList [x1,x2])

clause2pol :: [String] -> (PolF2, S.Set (PolF2))
clause2pol (c:cs) | c == "c" || c == "p" = (1, S.empty)
clause2pol cs = aux $ foldl' (\acc x -> disj (var' x) acc) (zerov,S.empty) cs
  where aux (a,b) = (expTo1 a,b)
        disj (x,v) (y,vs) | v == zerov = (y,vs)
                          | otherwise   = (x + y + x*y, S.insert v vs)

-------------------------------------------------------------------------------

-- | __(var' lit)__ is the pair /(p,v)/, where /p/ is the polynomial and /v/ is
-- the variable that corresponds (if it does) to the literal /lit/ (in DIMACS
-- format).
--
-- >>> var' "0"
-- (0,0)
-- >>> var' "1"
-- (x1,x1)
-- >>> var' "-1"
-- (x1+1,x1)

var' :: String -> (PolF2,PolF2)
var' "0"       = (zerov,zerov)
var' ('-':lit) = (1 + x,x)
                   where x = var ('x':lit)
var' lit       = (x,x)
                   where x = var ('x':lit)

-------------------------------------------------------------------------------

-- | __(dimacs2pols f)__ is the pair (/ps/,/vs/) where ps is the set of polynomials
-- wich corresponds to the formula in DIMACS format writed in the file /f/ and
-- /vs/ is the set of variables wich occurs in any polynomial.
--
-- >>> dimacs2pols "exDIMACS/easy/example1.txt"
-- (fromList [x1x2+x1+x2,1],fromList [x1,x2])
-- >>> dimacs2pols "exDIMACS/easy/example4.txt"
-- (fromList [x1x2+x1+x2,x1x2+x1+1,x1x2+x2+1,x1x2+1,1],fromList [x1,x2])

dimacs2pols f = do
  s <- readFile f
  print $ 
    (foldr (\x acc -> (insertPol ((clause2pol . words) x) acc))
           (S.empty,S.empty)) $
      lines $
        s
     where insertPol (a,b) (acc,vs) = (S.insert a acc, S.union vs b)
           
-------------------------------------------------------------------------------

-- | __(main f)__ is verified if the set of formulas in DIMACS format in the
-- file /f/ were satisfiable. Otherwise, /(main f)/ would return False.
--
-- >>> main "exDIMACS/easy/example1.txt"
-- True
-- >>> main "exDIMACS/easy/example4.txt"
-- False

main f = do
  s <- readFile f
  print $
    tool $
      (foldr (\x acc -> (insertPol ((clause2pol . words) x) acc))
             (S.empty,S.empty)) $
        lines $
          s
   where insertPol (a,b) (acc,vs) = (S.insert  a acc, S.union vs b)

-------------------------------------------------------------------------------

