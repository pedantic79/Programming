{-# LANGUAGE ExistentialQuantification #-}

module Lib
  ( filt
  , filtfoldr
  , filtfoldl
  , filtfusion
  , filtmatch
  , filtnaive
  ) where

import           Data.List (foldl')

filt = filter

filtfoldr p =
  foldr
    (\x a ->
       if p x
         then x : a
         else a)
    []

filtfoldl p = reverse .
  foldl
    (\a x ->
       if p x
         then x : a
         else a)
    []

filtmatch _ [] = []
filtmatch p (x:xs)
  | p x = x : filtmatch p xs
  | otherwise = filtmatch p xs

filtnaive p xs =
  if null xs
    then []
    else if p (head xs)
           then (head xs) : filtnaive p (tail xs)
           else filtnaive p (tail xs)

data Stream a =
  forall s. Stream (s -> Step a s) s

data Step a s
  = Done
  | Yield a s
  | Skip s

stream :: [a] -> Stream a
stream xs0 = Stream next xs0
  where
    next []     = Done
    next (x:xs) = Yield x xs

unstream :: Stream a -> [a]
unstream (Stream next0 s0) = unfold s0
  where
    unfold s =
      case next0 s of
        Done       -> []
        Skip s'    -> unfold s'
        Yield x s' -> x : unfold s'

filterfusion :: (a -> Bool) -> Stream a -> Stream a
filterfusion predicate (Stream next0 s0) = Stream noloop s0
  where
    noloop s =
      case next0 s of
        Done -> Done
        Skip s' -> Skip s'
        Yield x s'
          | predicate x -> Yield x s'
          | otherwise -> Skip s'

filtfusion :: (a -> Bool) -> [a] -> [a]
filtfusion predicate list = unstream . filterfusion predicate . stream $ list
