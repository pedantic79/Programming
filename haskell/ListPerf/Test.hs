module Main where

import Criterion.Main

noAcc m = noAcc' 0
  where noAcc' n      | n < m     = n:noAcc' (n + 1)
                      | otherwise = []

accPre m = accPre' 0 []
  where accPre' n acc | n < m     = accPre' (n + 1) (n:acc)
                      | otherwise = reverse acc

accApp m = accApp' 0 []
  where accApp' n acc | n < m     = accApp' (n + 1) (acc ++ [n])
                      | otherwise = acc

simple m = [0..m]

main = defaultMain
       [ bgroup "accApp" [ bench "10k"  $ whnf accApp 10000
                         , bench "50k"  $ whnf accApp 50000
                         , bench "500k" $ whnf accApp 500000
                         , bench "5m"   $ whnf accApp 5000000
                         ]
       , bgroup "accPre" [ bench "10k"  $ whnf accPre 10000
                         , bench "50k"  $ whnf accPre 50000
                         , bench "500k" $ whnf accPre 500000
                         , bench "5m"   $ whnf accPre 5000000
                         ]
       , bgroup "noAcc"  [ bench "10k"  $ whnf noAcc  10000
                         , bench "50k"  $ whnf noAcc  50000
                         , bench "500k" $ whnf noAcc  500000
                         , bench "5m"   $ whnf noAcc  5000000
                         ]
       ]

