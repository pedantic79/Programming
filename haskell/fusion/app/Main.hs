module Main where

import           Criterion.Main
import           Lib

f :: ((Integer -> Bool) -> [Integer] -> [Integer]) -> Integer -> [Integer]
f fn x = fn even [1 .. x]

main =
  defaultMain
    [ bgroup
        "1000"
        [ bench "filt" $ nf (f filt) 1000
        , bench "filtfoldr" $ nf (f filtfoldr) 1000
        , bench "filtfoldl" $ nf (f filtfoldl) 1000
        , bench "filtfusion" $ nf (f filtfusion) 1000
        , bench "filtmatch" $ nf (f filtmatch) 1000
        , bench "filtnaive" $ nf (f filtnaive) 1000
        ]
    ]
