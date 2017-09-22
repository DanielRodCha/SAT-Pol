module Main where

import qualified Data.Set as S
import Data.List (sortOn)
import ReadingF (variable2List, insertPol, clause2pol, counting)
import Tool

main = do
  s0 <- readFile "exDIMACS/medium/exampleSat1.txt"
  let s1 = variable2List $ (foldr (\x acc -> (insertPol ((clause2pol . words) x) acc))
             (S.empty,S.empty)) $ lines $ s0
  let s2 = map fst $ sortOn snd $ counting s1 []
  print $ tool (fst s1,s2)
