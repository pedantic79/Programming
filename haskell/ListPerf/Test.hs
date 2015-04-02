module Main where
import Criterion.Main

noAcc :: Integer -> [Integer]
noAcc m = noAcc' 0
  where noAcc' n      | n < m     = n:noAcc' (n + 1)
                      | otherwise = []

accPre :: Integer -> [Integer]
accPre m = accPre' 0 []
  where accPre' n acc | n < m     = accPre' (n + 1) (n:acc)
                      | otherwise = reverse acc

accApp :: Integer -> [Integer]
accApp m = accApp' 0 []
  where accApp' n acc | n < m     = accApp' (n + 1) (acc ++ [n])
                      | otherwise = acc

simple :: Integer -> [Integer]
simple m = [0..m]

main = defaultMain
       [ bgroup "accAppNf" [ bench "1"   $ nf accApp 1
                           , bench "10"  $ nf accApp 10
                           , bench "50"  $ nf accApp 50
                           ]
       , bgroup "accAccWh" [ bench "1"   $ whnf accPre 1
                           , bench "10"  $ whnf accPre 10
                           , bench "50"  $ whnf accPre  50
                           ]
       , bgroup "accPreNf" [ bench "1"   $ nf accPre 1
                           , bench "10"  $ nf accPre 10
                           , bench "50"  $ nf accPre 50
                           , bench "100" $ nf accPre 100
                           ]
       , bgroup "noAccNf"  [ bench "1"   $ nf noAcc  1
                           , bench "10"  $ nf noAcc  10
                           , bench "50"  $ nf noAcc  50
                           , bench "100" $ nf noAcc  100
                           ]
       ]

