module Main where
import Data.Char (intToDigit)
--import Text.Printf (printf)
import System.Environment (getArgs)

type Data = (Integer, Integer, Integer)

nextTerm :: Data -> Integer -> Data
nextTerm (acc, den, num) k = (k2 * (acc + num * 2), k2 * den, k * num)
                             where k2 = k * 2 + 1

eliminateDigit :: Data -> Integer -> Data
eliminateDigit (acc, den, num) d = (10 * (acc - den * d), den, 10 * num)

-- Convert Integer to Int
integerToInt :: Integer -> Int
integerToInt n = fromIntegral n::Int

integerToChar :: Integer -> Char
integerToChar = intToDigit . integerToInt
--integerToChar = head . show

-- Produce an infinite List containing digits of Pi
piListGen :: Data -> Integer -> Int -> Int -> String
piListGen t k i m | i >= m    = []
                  | num > acc = piListGen tNext kNext i m
                  | test2     = piListGen tNext kNext i m
                  | test      = ch : fmt ++ piListGen elim kNext iNext m
                  | otherwise = ch :        piListGen elim kNext iNext m
  where iNext = i + 1
        kNext = k + 1
        tNext@(acc, den, num) = nextTerm t kNext
        --q = (num * 3 + acc) `div` den
        --r = (num * 4 + acc) `div` den
        --test2 = q /= r        
        (q, r) = (num * 3 + acc) `divMod` den
        test2 = r + num >= den
        elim = eliminateDigit tNext q
        fmt = "\t:" ++ show iNext ++ "\n"
        test = iNext `mod` 10 == 0
        ch = integerToChar q


-- Produce an the pi as a String
piStrList :: Int -> String
piStrList m = piListGen (0, 1, 1) 0 0 m ++ fmt
  where left = m `mod` 10
        fmt | left /= 0 = replicate (10 - left) ' ' ++ "\t:" ++ show m ++ "\n"
            | otherwise = []

main = putStr . piStrList . read . head =<< getArgs
