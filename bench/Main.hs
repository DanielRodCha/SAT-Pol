module Main where

import Criterion.Main
import qualified ReadingF as RF

main = defaultMain [
   bgroup "dimacs2pols" [
       bench "easy1" $ whnf RF.dimacs2pols "exDIMACS/easy/example1.txt"
     , bench "easy2" $ whnf RF.dimacs2pols "exDIMACS/easy/example2.txt"
     , bench "easy3" $ whnf RF.dimacs2pols "exDIMACS/easy/example3.txt"
     , bench "easy4" $ whnf RF.dimacs2pols "exDIMACS/easy/example4.txt"
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
                    ]
  ]


