import Data.Bifunctor

-- Here we replace the getter/setter construct with a modifier function.
-- Where a Lens is simply a function that takes a function (a -> a) and
-- operates on s and returns the original value the modified structure
--
-- Note: s and a must have a relationship. As (a -> a) must be able to
-- fmap onto s.
type Lens s a = (a -> a) -> s -> (a, s)

-- We can see that relationship here, where (a -> a) can act upon a list [a]
-- ix :: Int -> (a -> a) -> [a] -> (a, [a])
ix :: Int -> Lens [a] a
ix index f list
  | index < 0   = error "ix: negative index"
  | null list   = error "ix: index too large"
  | h:t <- list = if index == 0
                  then (h,f h:t)
                  else second (h:) $ ix (index - 1) f t

-- Here the setter and getter, we use fst and snd to give us the appropriate
-- setter and getter.
--
-- The difference here is that we must always pass a value. In the case of
-- the getter, that is never evaluated so we can pass undefined. Although
-- passing id would be safer
--
-- In the setter, since we are replacing it with a constant we use the
-- const function
--
-- By using a function, we are able to modify the value
sampleGet = fst $ ix 3 undefined [0..10]
sampleSet = snd $ ix 4 (const 20) [0..10]
sampleMod = snd $ ix 4 (+40) [0..10]
