module Main where

import Criterion.Main

isPalin xs = p [] xs xs
  where
    p rev (a:as) (_:_:bs) = p (a:rev) as bs
    p rev (y:ys) [_] = rev == ys
    p rev ys [] = rev == ys

revPalin xs = xs == reverse xs

zipPalin xs = and $ zipWith (==) xs (reverse xs)

str n = take n $ repeat 'a'

main = defaultMain
       [ bgroup " isPalin" [ bench "  10" $ whnf  isPalin $ str   10
                           , bench " 100" $ whnf  isPalin $ str  100
                           , bench "1000" $ whnf  isPalin $ str 1000
                           ]
       , bgroup "revPalin" [ bench "  10" $ whnf revPalin $ str   10
                           , bench " 100" $ whnf revPalin $ str  100
                           , bench "1000" $ whnf revPalin $ str 1000
                           ]
       , bgroup "zipPalin" [ bench "  10" $ whnf zipPalin $ str   10
                           , bench " 100" $ whnf zipPalin $ str  100
                           , bench "1000" $ whnf zipPalin $ str 1000
                           ]
       ]
