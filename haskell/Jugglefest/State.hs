module State (assign) where
import qualified Control.Lens as Lens
import Control.Lens ((^.),(<|),(%=),(.=),_1,_head,at)
import Control.Monad (liftM,liftM2,when)
import Data.List (intercalate,sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes,listToMaybe)
import Types
import Lens

getJuggler :: JugglerName -> PDState (Maybe Juggler)
getJuggler jn = liftM (Map.lookup jn) $ Lens.use juggMap

getFirstToProcess :: PDState (Maybe Juggler)
getFirstToProcess = liftM listToMaybe $ Lens.use toProcess

getJuggFromCirc :: CircuitName -> PDState [Juggler]
getJuggFromCirc cn = do
  circMap <- Lens.use circuits
  case Map.lookup cn circMap of
   Nothing -> error $ "getJuggFromCirc: " ++ show cn
   Just js -> return js

getCircuitLen :: CircuitName -> PDState (Maybe Int)
getCircuitLen [] = return Nothing
getCircuitLen cn = do
  len <- liftM length $ getJuggFromCirc cn
  return (Just len)

addJuggler :: CircuitName -> Juggler -> PDState ()
addJuggler cn j = do
  circuits.at cn %= fmap (j<|)
  toProcess %= tail

removeLowJuggler :: CircuitName -> PDState Juggler
removeLowJuggler cn = do
  sList <- liftM sort $ getJuggFromCirc cn
  circuits.at cn .= return (tail sList)
  return $ Lens.over jCircDP tail (head sList)

assignJuggler :: PDState ()
assignJuggler = do
  mJ <- getFirstToProcess
  case mJ of
   Nothing -> return ()
   Just j  -> do
     let cn = j^.jCircDP._head._1
     addJuggler cn j
     mLen <- getCircuitLen cn
     case mLen of
      Nothing -> lost %= (j:)
      Just len -> do
        s <- Lens.use size
        when (len > s) $ do
          oldJ <- removeLowJuggler cn
          toProcess %= (oldJ:)
     assignJuggler

assignAllJugglers :: PDState ()
assignAllJugglers = do
  assignJuggler
  c <- liftM2 fn (Lens.use size) (Lens.use circuits)
  l <- Lens.use lost
  assignLostJugglers c l
  where
    fn s = Map.keys . Map.filter (\l -> length l < s)

assignLostJugglers :: [CircuitName] -> [Juggler] -> PDState ()
assignLostJugglers _ [] = return ()
assignLostJugglers cAll@(c:cs) (j:js) = do
  cLen <- getCircuitLen c
  s <- Lens.use size
  case cLen of
   Nothing -> error $ "assignLostJugglers: " ++ show cLen
   Just cl -> do
     circuits.at c %= fmap (j<|)
     if cl + 1 < s
       then assignLostJugglers cAll js
       else assignLostJugglers cs js

convertToLine :: [String] -> [Circuit] -> PDState [String]
convertToLine acc [] = return acc
convertToLine acc (c:cs) = do
  let cn = cName c
  jugglers <- getJuggFromCirc cn
  pristine <- mapM (getJuggler . jName) jugglers
  let line = cn ++ ' ' : intercalate "," (map show . catMaybes $ pristine)
  convertToLine (line : acc) cs

assign :: PDState [String]
assign = do
  assignAllJugglers
  c <- Lens.use circMap
  convertToLine [] (Map.elems c)
