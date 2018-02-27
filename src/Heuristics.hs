module Heuristics ( Heuristics
                  , monomialOrd
                  , frequency
                  , revFreq
                  , sizePol
                  , freqSize
                  , numVars
                  , freqNumVars) where

import Haskell4Maths
import F2

import Data.List (foldl', sortOn)
import qualified Data.Set as S

-- | The Heuristic data type indicates the order in which variables
-- are forgotten. 
type Heuristics = S.Set PolF2 -> [PolF2] -> [PolF2]

-- | (__ monomialOrd ps vs __) is the list of variables which occurs
-- in ps in the monomial order induced by the data type PolF2. For
-- example:
--
-- >>> [x1,x2] = map var ["x1","x2"] :: [PolF2]
-- >>> monomialOrd (S.fromList [x1,x2,x1+1]) [x1,x2]
-- [x1,x2]
monomialOrd :: Heuristics
monomialOrd ps vs = vs

-- | (__ frequency ps vs __) is the list of variables vs sorted by
-- frequency of occurrence in ps. For example:
--
-- >>> [x1,x2] = map var ["x1","x2"] :: [PolF2]
-- >>> frequency (S.fromList [x1,x2,x1+1]) [x1,x2]
-- [x2,x1]
frequency :: Heuristics
frequency ps vs = sortOn frequency vs
   where frequency v = length ( filter (== v) ps')
         ps' = foldl' (\acc p -> (vars p) ++ acc) [] ps

-- | (__ revFreq ps vs __) is the list of variables vs ordered in
-- reverse order of frequency in ps. For example:
--
-- >>> [x1,x2] = map var ["x1","x2"] :: [PolF2]
-- >>> revFreq (S.fromList [x1,x2,x1+1]) [x1,x2]
-- [x1,x2]
revFreq :: Heuristics
revFreq ps = (reverse . frequency ps)

sizePol :: Heuristics
sizePol ps vs = sortOn sizePol' vs
   where sizePol' v      = foldl' (\acc p -> (if elem v (vars p) then (sizePol'' p) else 0) + acc) 0 ps
         sizePol'' (V p) = sum $ map (\(Lex (M a b),k) -> length b) p


freqSize :: Heuristics
freqSize ps vs = sortOn (\v -> 1000*(frequency' v) + (sizePol' v)) vs
   where frequency' v = length $ filter (== v) ps''
         ps'  = foldl' (\acc p -> ((vars p,sizePol'' p):acc)) [] ps
         ps'' = concat $ map fst ps' 
         sizePol' v = foldl' (\acc (xs,n) -> (if (elem v xs) then n else 0) + acc) 0 ps'
         sizePol''  = length . show


numVars :: Heuristics
numVars ps vs = sortOn numVars' vs
    where numVars' v  = foldl' (\acc p -> (numVars'' p v) + acc) 0 ps

numVars'' p v | elem v vs = length vs
              | otherwise = 0
                  where vs = vars p

freqNumVars :: Heuristics
freqNumVars ps vs = sortOn (\v -> 1000*(frequency' v) + (numVars' v)) vs
   where frequency' v = length ( filter (== v) ps')
         ps' = foldl' (\acc p -> (vars p) ++ acc) [] ps
         numVars' v  = foldl' (\acc p -> (numVars'' p v) + acc) 0 ps