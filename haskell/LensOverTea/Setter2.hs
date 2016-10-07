{-# LANGUAGE RankNTypes #-}

import Data.Bifunctor
import Control.Monad
import Data.Functor.Identity

-- We'll rewrite our Lens type again. This time with a monad, because
-- monads are cool. So our type takes a monadic function, and an s
-- and returns the value s and the monad wrapped around s
type Lens s a = forall m . Monad m => (a -> m a) -> s -> (a, m s)

-- ix :: Int -> (a -> m a) -> [a] -> (a, m [a])
-- m can be any monad, and is dictated by the function passed to it.
-- if m is another [] then the type would be.
-- ix :: Int -> (a -> [a]) -> [a] -> (a, [[a]])
-- or if the m is Maybe
-- ix :: Int -> (a -> Maybe a) -> [a] -> (a, Maybe [a])
ix :: Int -> Lens [a] a
ix index f list
  | index < 0   = error "ix: negative index"
  | null list   = error "ix: index too large"
  | h:t <- list = if index == 0
                  then (h, liftM (:t) (f h))
                  else second (liftM (h:)) $ ix (index - 1) f t

-- Because of the get, we never evaluate (return id), so the type
-- checker never has to determine the type that we are lifting to
-- Interestingly, the type of (return id) is: Monad m => m (a -> a)
sampleGet = fst $ ix 3 (return id) [0..10]

-- Here we do have a type (a -> m [a]), but we return [[a]]
-- To get a [a] back we could use the Identity Monad, and then
-- use runIdentity to unwrap the Monad.
sampleSet = runIdentity . snd $ ix 4 (const (Identity 20)) [0..10]

-- Here to do I modification, our function must return a Monad
sampleMod = runIdentity . snd $ ix 4 (\x -> Identity x + 20) [0..10]

-- If we use the list Monad, we get interesting output because of
-- the propertires of the list Monad.
sampleSetL1 = snd $ ix 4 (const [20]) [0..10]
sampleSetL2 = snd $ ix 4 (\x -> [11..(11+x)]) [0..10]
