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
simple m = [1..m]

main = defaultMain
       [ bgroup "append"  [ bench "10"   $ nf accApp 10
                          , bench "50"   $ nf accApp 50
--                          , bench "250"  $ nf accApp 250
--                          , bench "1250" $ nf accApp 1250
                          ]
       , bgroup "prepend" [ bench "10"   $ nf accPre 10
                          , bench "50"   $ nf accPre 50
                          , bench "250"  $ nf accPre 250
                          , bench "1250" $ nf accPre 1250
                          ]
       , bgroup "noAccum" [ bench "10"   $ nf noAcc  10
                          , bench "50"   $ nf noAcc  50
                          , bench "250"  $ nf noAcc  250
                          , bench "1250" $ nf noAcc  1250
                          ]
       ]

