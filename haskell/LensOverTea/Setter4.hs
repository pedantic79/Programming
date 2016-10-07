{-# LANGUAGE RankNTypes #-}

import Data.Bifunctor
import Data.Functor.Compose
import Data.Functor.Identity
import Control.Applicative

-- Storey is similar to (x, f a). When you fmap onto it it modifies the
-- second value, and leaves the main value untouched.
data Storey x f a = Storey x (f a)
                  deriving Show

instance Functor f => Functor (Storey x f) where
  fmap f (Storey x fa) = Storey x (fmap f fa)

-- getStorey removes the Storey and returns just the pair
getStorey (Storey x y) = (x, y)

-- Storey stores two values, one is a value and another is Functor wrapped
exampleStorey = Storey 3 (Identity 9)

-- When you fmap a Storey it only applies it to the second value
-- We can use this structure for a lens, because we need to store
-- and modify possibly at the same time
exampleStoreyFmap = (+10) <$> exampleStorey -- Storey 3 (Identity 19)

-- Now that we have a way to store both the original value and modified
-- value, we can remove the backdoor to the original value. The original is:
-- type Lens s a = forall f . Functor f => (a -> f a) -> s -> (a, f s)
-- Now if we remove the first part of the pair we have our Lens
type Lens' s a = forall f . Functor f => (a -> f a) -> s -> f s

-- But nothing keeps us from changing types.
-- So we can go from s (source) to t (target) with an (a -> b)
-- The really nice thing is that Lens' are Lens
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- We can rewrite the weak Lens like this:
-- type Lens' s a = Lens s s a a

-- With the backdoor removed, we don't need to fetch original value
-- ix :: forall f . Functor f => Int -> (a -> f a) -> [a] -> f [a]
ix :: Int -> Lens' [a] a
ix index f list
  | index < 0   = error "ix: negative index"
  | null list   = error "ix: index too large"
  | h:t <- list = if index == 0
                  then (:t) <$> f h
                  else (h:) <$> ix (index - 1) f t

-- Using Storey we are able to get both the original and modified value
sampleSetGetS = getStorey  $ ix 3 (\x -> Storey x (Identity (+11)))  [1..10]

-- Storey is similar to the Compose Functor
-- type Storey x f = Compose ((,) x) f
sampleSetGetC = getCompose $ ix 3 (\x -> Compose (x, [1..x])) [1..10]

-- What do I do when you don't need any functor? Use Identity
-- The big benefit is that we can use the Lens as both a setter and a getter
-- the only difference is the function we pass to it.
sampleSetter = runIdentity $ ix 3 (\x -> Identity (x*88)) [1..10]

-- We can define this function that takes a Lens and a modification function
over :: Lens s t a b -> ((a -> b) -> s -> t)
over l f = runIdentity . l (Identity . f)

sampleOver1 = over (ix 3) (const 88) [1..10]
sampleOver2 = over (ix 3) (*4) [1..10]

-- What if we don't need a set?

sampleGetter = getConst $ ix 3 Const [0..10]
