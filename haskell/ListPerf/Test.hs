module Main where

import System.Environment (getArgs, getProgName)

test :: Integer -> Integer -> [Integer]
test m t
  | t == 0    = [0..m]
  | t == 1    = testNoAcc 0
  | t == 2    = testAccPrepend 0 []
  | otherwise = testAccAppend  0 []
  where testNoAcc n
	  | n < m     = n:testNoAcc (n + 1)
	  | otherwise = []
        testAccPrepend n acc
          | n < m     = testAccPrepend (n + 1) (n:acc)
          | otherwise = reverse acc
        testAccAppend n acc 
          | n < m     = testAccAppend (n + 1) (acc ++ [n])
          | otherwise = acc

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  putStrLn (if not (null args)
            then show $ test (f args 0) (f args 1) 
            else progName ++ " <num> [0|1|2|3]")
  where f args n
          | n  < length args = read (args !! n) :: Integer
          | otherwise        = 0
               

{-
ghc -Wall -fllvm -O rtsopts Test.hs

./Test 40000 0 +RTS -sstder >/dev/null

-}
