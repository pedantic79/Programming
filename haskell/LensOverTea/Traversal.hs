{-# LANGUAGE RankNTypes #-}
import Data.Functor.Identity
import Data.Functor.Const

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

over :: Lens s t a b -> ((a -> b) -> s -> t)
over l f = runIdentity . l (Identity . f)

set :: Lens s t a b -> b -> s -> t
set l f = runIdentity . l (\_ -> Identity f)

view :: Lens s t a b -> s -> a
view l = getConst . l Const

_1 :: Lens (a, x) (b, x) a b
_1 = \f (t, u) -> fmap (\v -> (v, u)) (f t)

-- _abs :: Real a => (a -> f a) -> a -> f a
_abs :: Real a => Lens' a a
_abs f n = update <$> f (abs n)
  where
    update x
      | x < 0     = error "Can't be negative"
      | otherwise = signum n * x

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = \f s -> set s <$> f (get s)

-- _abs :: Eq a => a -> (a -> f a) -> [a] -> f [a]
_all :: Eq a => a -> Lens' [a] a
_all ref = lens get set
  where
    get s     = ref
    set s new = map (\old -> if old == ref then new else old) s
