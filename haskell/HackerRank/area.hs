import Text.Printf (printf)
import Data.List (groupBy)

{-# ANN module "HLint: ignore Use fmap" #-}

-- Square
square :: (Num a, Num t, Eq t) => [(a, t)] -> [(a, t)]
square l = collect . groupByPow $ [(b*c, p+q) | (b, p) <- l, (c, q) <- l]
    where collect = map (foldr (\(a, p) (b, _) -> (a+b, p)) (0,0))
          groupByPow = groupBy (\(_, p1) (_, p2) -> p1 == p2)

evaluate :: Int -> [(Double, Double)] -> Double
evaluate x = sum . map (\(b, p) -> b*(fromIntegral x ** p))

integrate :: [(Int, Int)] -> [(Double, Double)]
integrate = map (\(b, p) -> let x = fromIntegral b
                                y = fromIntegral p
                            in (x/(y+1), y+1))

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = [area, vol]
  where fn = zip a b       -- zip the base and power together
        afn = integrate fn -- integrate fn
        vfn = integrate . square $ fn -- integrate the square of fn
        area = evaluate r afn - evaluate l afn  -- perform integral substraction
        vol = pi * (evaluate r vfn - evaluate l vfn) -- pi * Integral[fn^2, l, r]

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
