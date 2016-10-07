-- We'll start off with the most basic type of "Lens"
-- Here we have a record with a getter and setter
-- function.
data Lens s a = Lens
  { getter :: s -> a
  , setter :: a -> s -> s
  }

-- This is where the heavy lifting of the setter is done
-- We will recurse, until we find our correct index and
-- replace it with the new value
setIth :: Int -> a -> [a] -> [a]
setIth index new list
  | index < 0         = error "setIth: negative index"
  | null list         = error "setIth: index too large"
  | old:rest <- list  = if index == 0
                        then new : rest
                        else old : setIth (index-1) new rest

-- Here is our Lens, it takes an Int and returns us the Lens
ix :: Int -> Lens [a] a
ix i = Lens { getter = (!! i)
            , setter = setIth i
            }

-- We use the getter/setter functions along with the Lens to
-- perform the get/set action
sampleGet = getter (ix 3) [0..10]
sampleSet = setter (ix 4) 20 [0..10]
