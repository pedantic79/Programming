{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Data.Bifunctor
import Data.Functor.Compose
import Data.Functor.Identity

{-# ANN module "HLint: ignore Redundant lambda" #-}

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 = \f (t, u) -> fmap (\v -> (v, u)) (f t)

-- We can apply the value, then use fmap to unwrap the value from the Functor
-- and convert it to a Functor of a pair.
-- fmap (\x -> (x, 5)) (Identity 7) => Identity (7, 5)

-- With the TupleSections extension, you can write it like this:
-- _1 = \f (u, v) -> (, v) <$> f u

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 = \f (t, u) -> (t, ) <$> f u

-- Make a lens out of a getter and a setter.
-- lens :: (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = \f s -> set s <$> f (get s)
-- We use the getter to retrieve the value from s, then apply f to it.
-- we then use fmap and the setter to set that value

-- Combine 2 lenses to make a lens which works on Either. (It's a good idea
-- to try to use bimap for this, but it won't work, and you have to use
-- explicit case-matching. Still a good idea, tho.)
choosing :: Lens s1 t1 a b -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = \f s -> case s of
    Left  s1 -> Left  <$> l1 f s1
    Right s2 -> Right <$> l2 f s2

-- Modify the target of a lens and return the result. (Bonus points if you
-- do it without lambdas and defining new functions. There's also a hint
-- before the end of the section, so don't scroll if you don't want it.)
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f = l $ (\t -> (t, t)) . f

-- Modify the target of a lens, but return the old value.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f = l (\t -> (t, f t))

-- There's a () in every value. (No idea what this one is for, maybe it'll
-- become clear later.)
united :: Lens' s ()
united = \f a -> a <$ f ()
