{-# LANGUAGE Arrows #-}
import Control.Arrow ((>>>),(***),(&&&),returnA,Arrow(..))
import qualified Control.Category as C

data Circuit a b = Circuit (a -> b)

instance C.Category Circuit where
  id = Circuit id
  Circuit a . Circuit b = Circuit (a . b)
  
instance Arrow Circuit where
  arr f = Circuit f
  first (Circuit f) = Circuit $ \(x, y) -> (f x, y)

runCircuit :: Circuit t t1 -> t -> t1
runCircuit (Circuit f) x = f x

split :: Circuit Bool (Bool, Bool)
split = arr (id &&& id)

nandGate = arr (not . uncurry (&&))
invert = split >>> nandGate
orGate = (invert *** invert) >>> nandGate
norGate = orGate >>> invert
andGate = nandGate >>> invert
xorGate = (nandGate &&& orGate) >>> andGate
hAdder = (xorGate &&& andGate)
  
hAdder2 = proc i -> do
  o1 <- xorGate -< i
  o2 <- andGate -< i
  returnA -< (o1, o2)
