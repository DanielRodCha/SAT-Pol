module MainFunctions where

import Haskell4Maths (var
                     , vars
                     , zerov)
import F2 (PolF2)
import Logic
import Transformations ( projection
                       , theta)
import Heuristics
import Subsumption   -- not in use
import Analizador    (parseFProp)
import Preprocessing ( dimacs2Pols
                     , formulas2Pols)
import Saturation    ( forgetVarKB
                     , saturateKB)
import System.Environment
import qualified Data.Set as S

