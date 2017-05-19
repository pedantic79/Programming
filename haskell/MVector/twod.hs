import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad.ST
import Control.Monad
import Control.Monad.Primitive

image = [ [1, 2, 3]
        , [4, 5, 6]
        , [7, 8, 9]
        ]

toVector :: [[a]] -> V.Vector (V.Vector a)
toVector = V.fromList . fmap V.fromList

test :: V.Vector (V.Vector a) -> Int -> Int -> a -> V.Vector (V.Vector a)
test v r c x = runST $ do
  -- Here we thaw the inner row Vector and modify it
  -- we then copy to inner'
  inner <- V.thaw $ v V.! r
  VM.write inner c x
  inner' <- V.freeze inner

  -- Now thaw the outer Vector and overwrite the
  -- row vector with inner', and then freeze.
  outer <- V.thaw v
  VM.write outer r inner'
  V.freeze outer

create
  :: (PrimMonad m, Num a) =>
  [[a]] -> m (VM.MVector (PrimState m) a)
create lls =
  let r = length lls
      c = length . head $ lls
  in do
    v <- VM.new $ r * c + 2
    VM.write v 0 $ fromIntegral r
    VM.write v 1 $ fromIntegral c
    fill v 2 $ concat lls
    return v

fill
  :: PrimMonad m =>
  VM.MVector (PrimState m) a -> Int -> [a] -> m ()
fill _ _ [] = return ()
fill mv x (l:ls) = do
  VM.write mv x l
  fill mv (x + 1) ls

unf :: Num a => [[a]] -> V.Vector a
unf lls = runST $ do
  v <- create lls
  V.freeze v
