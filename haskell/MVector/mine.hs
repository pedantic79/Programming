import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad.ST
import Control.Monad

sweep = [[True, False,False,True]
        ,[False,False,True, False]
        ,[True, True, False,True]
        ]

create r c = do
  v <- VM.new $ r * c + 2
  let lls = replicate (r * c) 0
  VM.write v 0 $ fromIntegral r
  VM.write v 1 $ fromIntegral c
  fill v 2 lls
  return v
  where
    fill mv x lls = forM_ (zip [x..] lls) $ uncurry (VM.write mv)

upPair mv ps = do
  c <- VM.read mv 1
  up mv $ pair2idx c ps
  where
    up mv = mapM_ (modify mv (+1))
    pair2idx c = map (\(x,y) -> x * c + y + 2)

modify mv f i = do
  value <- VM.read mv i
  VM.write mv i $ f value

filterRange r c = filter (\(x,y) -> x >= 0 && y >= 0 && x < r && y < c)

genPairs r c = [(r-1,c-1), (r-1,c), (r-1,c+1)
               ,(r  ,c-1),          (r  ,c+1)
               ,(r+1,c-1), (r+1,c), (r+1,c+1)
               ]

-- unf :: [[Bool]] -> V.Vector Int
unf lls = runST $ do
  let (r,c) = getSizeL lls
  let mineVector = toVector lls
  v <- create r c
  forM_ [0..r-1] $ \i ->
    forM_ [0..c-1] $ \j ->
      when ((mineVector V.! i) V.! j)
        (upPair v . filterRange r c $ genPairs i j)
  V.freeze v
  where
    toVector = V.fromList . fmap V.fromList
    getSizeL ls = (length ls, length . head $ ls)

vector2List v = helper . V.toList . V.drop 2 $ v
  where
    helper [] = []
    helper ls = take c ls:helper (drop c ls)
    c = v V.! 1

test = vector2List . unf $ sweep
