{-# LANGUAGE RankNTypes #-}

import Data.Bifunctor
import Control.Applicative

type Lens s a = forall f . Functor f => (a -> f a) -> s -> (a, f s)

ix :: Int -> Lens [a] a
ix index f list
  | index < 0 = error "ix: negative index"
  | null list = error "ix: index too large"
  | h:t <- list = if (index == 0)
                  then (h, (:t) <$> (f h))
                  else second ((h:) <$>) $ ix (index - 1) f t

sampleGet = fst $ ix 3 (pure id) [0..10]
sampleSet = snd $ ix 4 (\x -> [11..(11+x)]) [0..10]
