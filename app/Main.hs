module Main where

import qualified Data.Set as S
import ReadingF
import Tool

main :: IO ()
main = do
  s <- readFile "exDIMACS/medium/exampleSat0.txt"
  print $
    toolN $
      (foldr (\x acc -> (S.insert ((clause2pol' . words) x) acc))
             S.empty) $
        lines s
