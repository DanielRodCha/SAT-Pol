module Main where

import qualified Data.Set as S
import Data.List (sortOn)
import ReadingF
import ToolCount

main = do
  s0 <- readFile "exDIMACS/hard/unsat250.cnf"
  let s1 = variable2RevList $ (foldr (\x acc -> (insertPol ((clause2pol . words) x) acc))
             (S.empty,S.empty)) $ lines $ s0
  let s2 = map fst $ sortOn snd $ counting s1 []
  print $ toolLC 0 (fst s1,s2)
