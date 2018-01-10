module Heuristics where

import Haskell4Maths (var
                     , vars)
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

