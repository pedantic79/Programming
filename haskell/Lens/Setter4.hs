{-# LANGUAGE RankNTypes #-}

import Data.Bifunctor
import Data.Functor.Compose
import Data.Functor.Identity
import Control.Applicative

data Storey x f a = Storey x (f a)
                  deriving Show

instance Functor f => Functor (Storey x f) where
  fmap f (Storey x fa) = Storey x (fmap f fa)

getStorey (Storey x y) = (x, y)  

type Lens s a = Functor f => (a -> f a) -> s -> f s

ix :: Int -> Lens [a] a
ix index f list
  | index < 0 = error "ix: negative index"
  | null list = error "ix: index too large"
  | h:t <- list = if (index == 0)
                  then (:t) <$> f h
                  else (h:) <$> ix (index - 1) f t

sampleSetGet = getStorey $ ix 3 (\x -> Storey x [1..x]) [1..10]
sampleSetGet' = getCompose $ ix 3 (\x -> Compose (x, [1..x])) [1..10]

sampleSetter = runIdentity $ ix 3 (\x -> Identity (x*88)) [1..10]
sampleGetter = getConst $ ix 3 (\x -> Const x) [0..10]
