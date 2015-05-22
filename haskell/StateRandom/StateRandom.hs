import Control.Monad.State
import System.Random

rollDie :: State StdGen Int
rollDie = do
  gen <- get
  let (n, newG) = randomR (1, 6) gen
  put newG
  return n

rollDie2 :: State StdGen Int
rollDie2 = get >>= \gen -> let (n, newG) = randomR (1, 6) gen
                           in put newG >> return n

rollDieT :: StateT StdGen IO Int
rollDieT = do
  gen <- get
  let (n, newG) = randomR (1, 6) gen
  lift $ print newG
  put newG
  return n


rollDice :: State StdGen (Int, Int)
rollDice = liftM2 (,) rollDie rollDie2

rollDice7 :: State StdGen (Int, Int, Int)
rollDice7 = liftM (,,) rollDie `ap` rollDie `ap` rollDie


clumsyDice :: StdGen -> (Int, Int)
clumsyDice g = (d1, d2)
  where (d1, g1) = randomR (1, 6) g
        (d2, g2) = randomR (1, 6) g1

test1 = evalState rollDie (mkStdGen 0)
test2 = evalStateT rollDieT (mkStdGen 0)
