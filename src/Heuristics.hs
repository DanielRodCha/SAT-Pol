{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Heuristics
    ( heuristics
    ) where

import PolAux
import Math.CommutativeAlgebra.Polynomial(vars)

import qualified Data.Set as S
import Data.List (foldl', sortOn)

-------------------------------------------------------------------------------
-- | The data type Heuristic is the rule that dictates in which order variables
-- are chosen.



-------------------------------------------------------------------------------

frequency ps v = length $ filter (== v) $ foldl' (\acc p -> (vars p) ++ acc) [] ps

heuristics ps vs = sortOn (frequency ps) vs

