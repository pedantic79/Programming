
> import Control.Lens
> import Control.Monad.State

This is the standard way we would use the State Monad
ghci> execState (tupleMap (+4) (+5)) (3,4)
(7,9)

> tupleMap :: (Int -> Int) -> (Int -> Int) -> State (Int,Int) ()
> tupleMap f g = do
>  (a,b) <- get
>  let m = (f a, g b)
>  put m

Using lenses we can focus on the individual part of the tuple
and apply the function to it. tupleMap' is identical to
tupleMap.
ghci> execState (tupleMap' (+4) (+5)) (3,4)
(7,9)

> tupleMap' :: (Int -> Int) -> (Int -> Int) -> State (Int,Int) ()
> tupleMap' f g = do
>   _1 %= f
>   _2 %= g

Here we define a square operation. Later we will call it to show
how we can compose two of these functions. Notice that the do is not
required.
ghci> execState sq (5,6)
(25,36)

> sq :: State (Int,Int) ()
> sq = both ^= 2

Here we call sq between applying the functions to the tuples
This is how the State is actually threaded throughout our calls
ghci> execState (tSqMap (*3) (*2)) (5,6)
(225,72)

First we apply (*3) to the fst value. State: (15,6)
Then we square both. State: (225,36)
Then we apply (*2) to the snd value. State: (225,72)

> tSqMap :: (Int -> Int) -> (Int -> Int) -> State (Int,Int) ()
> tSqMap f g = do
>   _1 %= f
>   sq
>   _2 %= g

With a more complex structure like this tuple of tuple we can
easily square the fst values of both tuples and cube the
values of the snd values of both tuples.

I will refer to the outermost elements as tuple (X,Y), and
the innnermost elements as ((A,B),(C,D)) to keep things as simple
as possible.

both . _1 focuses on elements A and C. the both lets us act on both
X and Y, and then composing it with _1 will get us A and C.

Similarly, both . _2 will get us B and D.

ghci> execState tupSq ((5,6),(2,4))
((25,216),(4,64))

> tupSqCub :: State ((Int,Int),(Int,Int)) ()
> tupSqCub = do
>  both . _1 ^= 2
>  both . _2 ^= 3

