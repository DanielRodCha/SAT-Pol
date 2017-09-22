{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module ReadingF where

import Data.List
import Data.Char (isSpace)
import Data.Foldable (sum, product)
import System.Environment

import qualified Data.Set as S

import PolExamples (x1,x2,x3)
import PolAux (PolF2, expTo1, var, zerov, vars)
import Math.CommutativeAlgebra.Polynomial (lexvar)
import Tool (tool, next)
import ToolCount (toolCount, toolLC)

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

-- | __(variable2List (a,b))__ returns a pair which first element remains
-- unchanged and the second is the list of elements of set /b/. For example,
-- 
-- >>> variable2List (1,S.empty)
-- (1,[])
-- >>> variable2List (x1,S.fromList[0,2])
-- (x1,[0,2])

variable2List :: (t, S.Set a) -> (t, [a])
variable2List (a,b) = (a,S.toList b)

-------------------------------------------------------------------------------

-- | __(insertPol (a,b) (acc,vs))__ is a pair which first element is set /acc/
-- plus element /a/ and the second is the union of set /vs/ and set /b/. For
-- example,
--
-- >>> insertPol (x1,S.fromList[x1]) (S.empty,S.empty) 
-- (fromList [x1],fromList [x1])
-- >>> insertPol (x1*x2,S.fromList[x1,x2]) (S.fromList[x1],S.fromList[x1])
-- (fromList [x1x2,x1],fromList [x1,x2])
-- >>> insertPol (x1,S.fromList[x1]) (S.fromList[x1*x2],S.fromList[x1,x2])
-- (fromList [x1x2,x1],fromList [x1,x2])

insertPol :: (PolF2, S.Set PolF2) -> (S.Set PolF2, S.Set PolF2) ->
             (S.Set PolF2, S.Set PolF2)
insertPol (a,b) (acc,vs) = (S.insert  a acc, S.union vs b)

-------------------------------------------------------------------------------

-- | __(hasV v p)__ is 1 if /p/ has the variable /v/ otherwise it is 0. For
-- example,
--
-- >>> hasV x1 x1
-- 1
-- >>> hasV x2 (x1+1)
-- 0
-- >>> hasV x1 (x1*x2+1)
-- 1

hasV :: PolF2 -> PolF2 -> Int
hasV v p | elem v (vars p) = 1
         | otherwise       = 0

-------------------------------------------------------------------------------

-- | __(counting (ps,vvs) cs)__ is a list of ordered pairs in which the first
-- element is a variable from /vvs/ and the second indicates in how many
-- polynomials of the set /ps/ this variable occurs. For example,
--
-- >>> counting (S.empty, []) []
-- []
-- >>> counting (S.fromList [x1*x2,x1,x1+1,x2], [x1,x2]) []
-- [(x2,2),(x1,3)]
-- >>> counting (S.fromList [x1], [x1,x2,x3]) []
-- [(x3,0),(x2,0),(x1,1)]

counting :: (S.Set(PolF2),[PolF2]) -> [(PolF2,Int)] -> [(PolF2,Int)]
counting (ps,[]) cs     = cs
counting (ps,(v:vs)) cs = counting (ps,vs)
  ([(v, (foldl (\acc x -> (hasV v x)+acc) 0 (S.toList ps)))] ++ cs)

-------------------------------------------------------------------------------

-- | __(dimacs2pols f)__ is the pair (/ps/,/vs/) where ps is the set of polynomials
-- wich corresponds to the formula in DIMACS format writed in the file /f/ and
-- /vs/ is the list of variables wich occurs in any polynomial.
--
-- >>> dimacs2pols "exDIMACS/easy/example1.txt"
-- (fromList [x1x2+x1+x2,1],[x1,x2])
-- >>> dimacs2pols "exDIMACS/easy/example2.txt"
-- (fromList [x1x2+x1+x2,x1x2+x1+1,1],[x1,x2])
-- >>> dimacs2pols "exDIMACS/easy/example3.txt"
-- (fromList [x1x2+x1+x2,x1x2+x1+1,x1x2+x2+1,1],[x1,x2])
-- >>> dimacs2pols "exDIMACS/easy/example4.txt"
-- (fromList [x1x2+x1+x2,x1x2+x1+1,x1x2+x2+1,x1x2+1,1],[x1,x2])

dimacs2pols f = do
  s0 <- readFile f
  let (s1,s2) = variable2List $ (foldr (\x acc -> (insertPol ((clause2pol . words) x) acc))
             (S.empty,S.empty)) $ lines $ s0
  let s3 = map fst $ sortOn snd $ counting (s1,s2) []
  print $ (s1, s2)

-------------------------------------------------------------------------------

-- | __(dimacs2pols f)__ is the pair (n,m) where /n/ is the size of the set of
-- polynomials wich corresponds to the formula in DIMACS format writed in the
-- file /f/ and /m/ is the length of the list of variables wich occurs in any
-- polynomial.
--
-- >>> dimacs2polsSize "exDIMACS/easy/example1.txt"
-- (2,2)
-- >>> dimacs2polsSize "exDIMACS/easy/example2.txt"
-- (3,2)
-- >>> dimacs2polsSize "exDIMACS/easy/example3.txt"
-- (4,2)
-- >>> dimacs2polsSize "exDIMACS/easy/example4.txt"
-- (5,2)
-- >>> dimacs2polsSize "exDIMACS/hard/sat100.cnf"
-- (431,100)
-- >>> dimacs2polsSize "exDIMACS/hard/sat250.cnf"
-- (1066,250)

dimacs2polsSize f = do
  s0 <- readFile f
  let s1 = variable2List $ (foldr (\x acc -> (insertPol ((clause2pol . words) x) acc))
             (S.empty,S.empty)) $ lines $ s0
  let s2 = map fst $ sortOn snd $ counting s1 []
  print $ (S.size (fst s1),length (s2))
           
-------------------------------------------------------------------------------

-- | __(main f)__ is verified if the set of formulas in DIMACS format in the
-- file /f/ were satisfiable. Otherwise, /(main f)/ would return False.
--
-- >>> main "exDIMACS/easy/example1.txt"
-- True
-- >>> main "exDIMACS/easy/example2.txt"
-- True
-- >>> main "exDIMACS/easy/example3.txt"
-- True
-- >>> main "exDIMACS/easy/example4.txt"
-- False
-- >>> main "exDIMACS/medium/exampleSat0.txt"
-- True
-- >>> main "exDIMACS/medium/exampleSat1.txt"
-- True

main f = do
  s0 <- readFile f
  let s1 = variable2List $ (foldr (\x acc -> (insertPol ((clause2pol . words) x) acc))
             (S.empty,S.empty)) $ lines $ s0
  let s2 = map fst $ sortOn snd $ counting s1 []
  print $ tool (fst s1,s2)

-- | __(mainCount f)__ returns a pair where first component is verified if the
-- set of formulas in DIMACS format in the file /f/ were satisfiable; and the
-- second component counts how many times the delta rule has been executed.
--
-- >>> mainCount "exDIMACS/easy/example1.txt"
-- (True,1)
-- >>> mainCount "exDIMACS/easy/example2.txt"
-- (True,3)
-- >>> mainCount "exDIMACS/easy/example3.txt"
-- (True,7)
-- >>> mainCount "exDIMACS/easy/example4.txt"
-- (False,12)
-- >>> mainCount "exDIMACS/medium/exampleSat0.txt"
-- (True,114)
-- >>> mainCount "exDIMACS/medium/exampleSat1.txt"
-- (True,142)
-------------------------------------------------------------------------------

mainCount f = do
  s0 <- readFile f
  let s1 = variable2List $ (foldr (\x acc -> (insertPol ((clause2pol . words) x) acc))
             (S.empty,S.empty)) $ lines $ s0
  let s2 = map fst $ sortOn snd $ counting s1 []
  print $ toolLC 0 (fst s1,s2)
