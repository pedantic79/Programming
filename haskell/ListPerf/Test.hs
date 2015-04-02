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
       [ bgroup "accApp" [ bench "1"   $ whnf accApp 1
                         , bench "10"  $ whnf accApp 10
                         , bench "50"  $ whnf accApp 50
                         , bench "100" $ whnf accApp 100
                         ]
       , bgroup "accPre" [ bench "1"   $ whnf accPre 1
                         , bench "10"  $ whnf accPre 10
                         , bench "50"  $ whnf accPre 50
                         , bench "100" $ whnf accPre 100
                         ]
       , bgroup "noAcc"  [ bench "1"   $ whnf noAcc  1
                         , bench "10"  $ whnf noAcc  10
                         , bench "50"  $ whnf noAcc  50
                         , bench "100" $ whnf noAcc  100
                         ]
       ]

