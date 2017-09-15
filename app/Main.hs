module Main where

import qualified Data.Set as S
import ReadingF
import Tool

main :: IO ()
main = do
  s <- readFile "exDIMACS/medium/exampleSat1.txt"
  print $
    tool $
      (foldr (\x acc -> (insertPol ((clause2pol . words) x) acc))
             (S.empty,S.empty)) $
        lines $
          s
