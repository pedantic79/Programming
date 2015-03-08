{-# LANGUAGE BangPatterns #-}
-- Shamelessly stolen from:
-- http://en.literateprograms.org/Pi_with_Machin's_formula_(Haskell)
module Main where
import Control.Parallel (par, pseq)
import System.Environment (getArgs, getProgName)

-- arccot x = 1/x - 1/3x^3 + 1/5x^5 - 1/7x^7 ...
{- We will replace 1 with a large value 10^n, called unity.
   e.g. 1000000/3 = 333333, when we divide by 10000000 we get .33333
   This allows us to use div (integer division) and still maintain
   percision.

   sum is our accumulator

   xpower is to keep track of the unity/x^n value. We precompute it for the
   next iteration. So the first time we divide once, and then every iteration
   through we just divide by x^2. e.g. unity/x then unity/x^3 then unity/x^5

   mult is the multiplier in the denominator. 

   sign keeps track of if we are adding or multiplying
-}
arccot x unity = par pos (pseq neg (pos + neg))
  where pos = arccot' 0 (unity `div` x) 1 (+) 
        neg = arccot' 0 (unity `div` x3) 3 (-)
        arccot' !sum xpower mult sfunc
          | term == 0 = sum
          | otherwise = arccot' (sfunc sum term) next (mult + 4) sfunc
          where term = xpower `div` mult
                next = xpower `div` x4
        x3 = x * x * x
        x4 = x3 * x

-- pi/4 = 4 * arccot 5 - arccot 239
{- We give ourselves a 10^10 padding. This way all of are digits are
   accurate. If we don't, then the last few digits maybe off because
   we haven't converged.
-}
machinPi digits = pi' `div` (10 ^ 10)
  where unity = 10 ^ (digits+10)
        pi'   = 4 * (4 * arccot 5 unity - arccot 239 unity)

-- ./PiParallel <digits> +RTS -sstderr -N2
main = do
  args <- getArgs
  progName <- getProgName
  if length args == 1
    then print $ machinPi $ getDigits args
    else putStrLn $ progName ++ " <digits>"
  where getDigits args = read (head args) :: Integer
    
     
