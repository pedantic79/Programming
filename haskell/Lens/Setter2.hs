{-# LANGUAGE RankNTypes #-}

import Data.Bifunctor
import Control.Monad

type Lens s a = Monad m => (a -> m a) -> s -> (a, m s)

ix :: Int -> Lens [a] a
ix index f list
  | index < 0 = error "ix: negative index"
  | null list = error "ix: index too large"
  | h:t <- list = if (index == 0)
                  then (h, liftM (:t) (f h))
                  else second (liftM (h:)) $ ix (index - 1) f t

sampleGet = fst $ ix 3 (return id) [0..10]
sampleSet = snd $ ix 4 (\x -> [11..(11+x)]) [0..10]
