module Main where

import Criterion.Main
import qualified ReadingF as RF

main = defaultMain [
   bgroup "dimacs2pols" [
       bench "easy1" $ whnf RF.dimacs2pols "exDIMACS/easy/example1.txt"
     , bench "easy2" $ whnf RF.dimacs2pols "exDIMACS/easy/example2.txt"
     , bench "easy3" $ whnf RF.dimacs2pols "exDIMACS/easy/example3.txt"
     , bench "easy4" $ whnf RF.dimacs2pols "exDIMACS/easy/example4.txt"
     , bench "medium0" $ whnf RF.dimacs2pols "exDIMACS/medium/exampleSat0.txt"
     , bench "medium1" $ whnf RF.dimacs2pols "exDIMACS/medium/exampleSat1.txt"
     ]
  -- , bgroup "var'" [
  --      bench "0"  $ whnf RF.var' "0"
  --    , bench "1"  $ whnf RF.var' "1"
  --    , bench "-1" $ whnf RF.var' "-1"
  --    , bench "0"  $ whnf RF.var' "10"
  --    , bench "1"  $ whnf RF.var' "-10"
  --    , bench "-1" $ whnf RF.var' "100"
  --                        ]
   , bgroup "main" [
       bench "easy1" $ whnf RF.main "exDIMACS/easy/example1.txt"
     , bench "easy2" $ whnf RF.main "exDIMACS/easy/example2.txt"
     , bench "easy3" $ whnf RF.main "exDIMACS/easy/example3.txt"
     , bench "easy4" $ whnf RF.main "exDIMACS/easy/example4.txt"
     , bench "medium0" $ whnf RF.main "exDIMACS/medium/exampleSat0.txt"
     , bench "medium1" $ whnf RF.main "exDIMACS/medium/exampleSat1.txt"
     ]
   , bgroup "mainN" [
       bench "easy1" $ whnf RF.mainN "exDIMACS/easy/example1.txt"
     , bench "easy2" $ whnf RF.mainN "exDIMACS/easy/example2.txt"
     , bench "easy3" $ whnf RF.mainN "exDIMACS/easy/example3.txt"
     , bench "easy4" $ whnf RF.mainN "exDIMACS/easy/example4.txt"
     , bench "medium0" $ whnf RF.mainN "exDIMACS/medium/exampleSat0.txt"
     , bench "medium1" $ whnf RF.mainN "exDIMACS/medium/exampleSat1.txt"
     ]
   , bgroup "mainList" [
       bench "easy1" $ whnf RF.mainList "exDIMACS/easy/example1.txt"
     , bench "easy2" $ whnf RF.mainList "exDIMACS/easy/example2.txt"
     , bench "easy3" $ whnf RF.mainList "exDIMACS/easy/example3.txt"
     , bench "easy4" $ whnf RF.mainList "exDIMACS/easy/example4.txt"
     , bench "medium0" $ whnf RF.mainList "exDIMACS/medium/exampleSat0.txt"
     , bench "medium1" $ whnf RF.mainList "exDIMACS/medium/exampleSat1.txt"
     ]
  ]


