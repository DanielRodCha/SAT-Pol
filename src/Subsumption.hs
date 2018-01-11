module Subsumption where

import Haskell4Maths ((%%))
import F2 (PolF2)

import qualified Data.Set as S

isDivisibleBy :: PolF2 -> PolF2 -> Bool
isDivisibleBy _ 0 = False --bug
x `isDivisibleBy` y = x %% [y] == 0

removeDivisors :: PolF2 -> S.Set PolF2 -> S.Set PolF2
removeDivisors x = S.filter (\y -> not (x `isDivisibleBy` y))