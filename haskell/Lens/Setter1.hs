import Data.Bifunctor

type Lens s a = (a -> a) -> s -> (a, s)

ix :: Int -> Lens [a] a
ix index f list
  | index < 0 = error "ix: negative index"
  | null list = error "ix: index too large"
  | h:t <- list = if (index == 0)
                  then (h,f h:t)
                  else second (h:) $ ix (index - 1) f t

sampleGet = fst $ ix 3 undefined [0..10]
sampleSet = snd $ ix 4 (const 20) [0..10]
