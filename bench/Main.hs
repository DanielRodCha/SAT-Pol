import Criterion.Main
import qualified ReadingF as RF

main = defaultMain [
  bgroup "dimacs2pols" [
      bench "easy1" $ whnfIO $ RF.dimacs2pols "exDIMACS/easy/example1.txt"
    , bench "easy2" $ whnfIO $ RF.dimacs2pols "exDIMACS/easy/example2.txt"
    , bench "easy3" $ whnfIO $ RF.dimacs2pols "exDIMACS/easy/example3.txt"
    , bench "easy4" $ whnfIO $ RF.dimacs2pols "exDIMACS/easy/example4.txt"
                        ]
  ]


