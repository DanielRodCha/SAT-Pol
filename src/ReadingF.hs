{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module ReadingF where

import Data.List
import Data.Char (isSpace)
import Data.Foldable (sum, product)
import System.Environment

import qualified Data.Set as S

import PolAux (PolF2, expTo1, var, zerov)
import Tool (tool)


-- main1 :: FilePath -> IO ()
-- main1 f = do
--   s <- readFile f
--   print
--     $ nub
--     $ foldr (\x acc -> (((cleanExp . varFold . words) x):acc)) []
--     $ lines
--     $ s

-- -- main "/Users/danielrodriguezchavarria/Desktop/300/ReadingFiles/fil3.txt"

-- main2 :: FilePath -> IO ()
-- main2 f = do
--   s <- readFile f
--   print
--     $ tool
--     $ nub
--     $ foldr (\x acc -> (((cleanExp . varFold . words) x):acc)) []
--     $ lines
--     $ s 

cleanP (a,b) = (expTo1 a,b)

insertP (a,b) (acc,vs) = (S.insert a acc, S.union vs b)

insertP' (a,b) (acc,vs) = (S.insert (a,b) acc, S.union vs b)

main3 f = do
  s <- readFile f
  print $ (foldr (\x acc -> (S.insert ((expTo1 . varFold . words) x) acc)) S.empty) $ lines $ s

main3P f = do
  s <- readFile f
  print $ snd $ (foldr (\x acc -> (insertP ((cleanP . varFoldP . words) x) acc)) (S.empty,S.empty)) $ lines $ s
-------------------------------------------------------------------------------

-- main2 "/Users/danielrodriguezchavarria/Desktop/300/ReadingFiles/fil3.txt"

-- main4 f = do
--   s <- readFile f
--   print $ tool1 $ (foldr (\x acc -> (S.insert ((expTo1 . varFold . words) x) acc)) S.empty) $ lines $ s

main4P f = do
  s <- readFile f
  print $ tool $ (foldr (\x acc -> (insertP ((cleanP . varFoldP . words) x) acc)) (S.empty,S.empty)) $ lines $ s

-- main4SP f = do
--   s <- readFile f
--   print $ toolSP $ (foldr (\x acc -> (insertP' ((cleanP . varFoldP . words) x) acc)) (S.empty,S.empty)) $ lines $ s

-------------------------------------------------------------------------------
  
-- Si es FND usar:

-- divide :: [String] -> [[String]]
-- divide [] = [[]]
-- divide ((x:xs):xss) | x == '-'  = map (xs:) yss ++ yss
--                     | otherwise = map ((x:xs):) yss
--   where yss = divide xss

-------------------------------------------------------------------------------

-- Si es FNC usar:

varFold :: [String] -> PolF2
varFold (x:xs) | x == "c" || x == "p" = 1
varFold xs = foldl' (\acc x -> disj (var' x) acc) zerov xs

varFoldP :: [String] -> (PolF2, S.Set (PolF2))
varFoldP (x:xs) | x == "c" || x == "p" = (1, S.empty)
varFoldP xs = foldl' (\acc x -> disjP (var'P x) acc) (zerov,S.empty) xs

-------------------------------------------------------------------------------

var' "0"      = zerov
var' ('-':xs) = 1 + var xs
var' x        = var x


var'P "0"      = (zerov,zerov)
var'P ('-':xs) = (1 + x,x)
                   where x = var xs
var'P xs       = (x,x)
                   where x = var xs    

-------------------------------------------------------------------------------

disj :: Num a => a -> a -> a
disj x y = x + y + x*y


disjP (x,v) (y,vs) | v == zerov = (y,vs)
                   | otherwise   = (x + y + x*y, S.insert v vs)
-------------------------------------------------------------------------------
