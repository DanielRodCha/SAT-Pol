{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Heuristics
    ( heuristics
    ) where

import PolAux
import Examples

import qualified Data.Set as S
import Data.List (foldl', sortOn)

-------------------------------------------------------------------------------
-- | The data type Heuristic is the rule that dictates in which order variables
-- are chosen.



-------------------------------------------------------------------------------

frequency ps v = length $ filter (== v) $ foldl' (\acc p -> (vars p) ++ acc) [] ps

frequencies ps vs = foldl' (\acc v -> (v,frequency ps v):acc) [] vs

heuristics ps vs = sortOn (frequency ps) vs
  --foldr (\x acc -> ((fst x):acc)) [] $ sortOn snd $ frequencies ps vs



x1 :: PolF2
x1 = var "x1"

(s0,s0') = fileSat0'''
