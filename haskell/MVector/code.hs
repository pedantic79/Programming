import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad.ST

vl = V.fromList [1..10]

updateTest v idx = V.accum (+) v [(idx, 1)]

test :: ST s (VM.MVector s Int)
test = do
  -- v <- VM.new 10         -- create vector (sparse)
  v <- VM.replicate 10 0    -- previous vector has unitialized values
  VM.write v 3 2000         -- modify it
  VM.write v 4 3000
  x <- VM.read v 3          -- access it
  VM.write v 4 (x+1)
  VM.modify v (+2) 1        -- modify it
  return v                  -- return it

-- This instead returns a immutable Vector
test2 :: ST s (V.Vector Int)
test2 = do
  v <- test                  -- call test, which performs the allocation
  V.freeze v                 -- return immutable vector

-- This returns a plain pure immutable Vector
test3 :: V.Vector Int
test3 = runST test2

-- Pass a immutable vector in, and thaw/modify/freeze
test4 :: Num a => V.Vector a -> V.Vector a
test4 vs = runST $ do
  v <- V.thaw vs
  VM.write v 3 1000
  V.freeze v
