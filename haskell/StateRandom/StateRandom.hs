import Control.Monad.State
import System.Random

rollDie :: State StdGen Int
rollDie = do
  gen <- get
  let (n, newG) = randomR (1, 6) gen
  put newG
  return n

rollDieT :: StateT StdGen IO Int
rollDieT = do
  gen <- get
  let (n, newG) = randomR (1, 6) gen
  lift $ print newG
  put newG
  return n

rollDieIO :: IO Int
rollDieIO = randomRIO (1, 6)

rollDice :: State StdGen (Int, Int)
rollDice = liftM2 (,) rollDie rollDie

rollDiceT :: StateT StdGen IO (Int, Int)
rollDiceT = do
  a <- rollDieT
  lift $ print a
  b <- rollDieT
  lift $ print b
  return (a,b)


clumsyDice :: StdGen -> (Int, Int)
clumsyDice g = (d1, d2)
  where (d1, g1) = randomR (1, 6) g
        (d2, g2) = randomR (1, 6) g1

test1 = evalState rollDie (mkStdGen 0)
test2 = evalStateT rollDieT (mkStdGen 0)
test3 = evalState rollDice (mkStdGen 0)
test4 = evalStateT rollDiceT (mkStdGen 0)
