import Control.Lens
import Control.Monad.State

tupleMap :: (Int -> Int) -> (Int -> Int) -> State (Int,Int) ()
tupleMap f g = do
  (a,b) <- get
  let m = (f a, g b)
  put m

-- ghci> execState (tupleMap (+4) (+5)) (3,4)
-- (7,9)

sq :: State (Int,Int) ()
sq = do
  both ^= 2

-- ghci> execState sq (5,6)
-- (25,36)


tMap :: (Int -> Int) -> (Int -> Int) -> State (Int,Int) ()
tMap f g = do
  sq
  _1 %= f
  _2 %= g

-- fst we square 5 then multiply by 3
-- snd we square 6 then multiply by 2
-- ghci> execState (tMap (*3) (*2)) (5, 6)
-- (75,72)

tupSqCub :: State ((Int,Int),(Int,Int)) ()
tupSqCub = do
  both . _1 ^= 2
  both . _2 ^= 3

-- ghci> execState tupSq ((5,6),(2,4))
-- ((25,216),(4,64))
