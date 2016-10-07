{-# LANGUAGE RankNTypes #-}

import Data.Bifunctor
import Data.Functor.Identity
--import Control.Applicative

-- Do we really need to use monads? No! All monads are functors,
-- so let's define it so that they are functors

type Lens s a = forall f . Functor f => (a -> f a) -> s -> (a, f s)

-- Replace calls to liftM with the infix fmap <$>
-- ix :: Int -> (a -> f a) -> [a] -> (a, f [a])
ix :: Int -> Lens [a] a
ix index f list
  | index < 0   = error "ix: negative index"
  | null list   = error "ix: index too large"
  | h:t <- list = if index == 0
                  then (h, (:t) <$> f h)
                  else second ((h:) <$>) $ ix (index - 1) f t

-- These work much the same way as when they were Monads
sampleGet = fst $ ix 3 (pure id) [0..10]
sampleSet = runIdentity . snd $ ix 4 (const (Identity 20)) [0..10]
sampleMod = runIdentity . snd $ ix 4 (\x -> Identity x+100) [0..10]

sampleSet2 = snd $ ix 4 (\x -> [11..(11+x)]) [0..10]
