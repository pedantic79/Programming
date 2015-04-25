module State (assign) where
import qualified Control.Lens as Lens
import Control.Lens ((^.),(<|),(%=),(.=),at,_1,_head)
import Control.Monad (when)
import Data.List (intercalate,sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Types

getJuggler :: JugglerName -> PDState (Maybe Juggler)
getJuggler jn = do
  j <- Lens.use juggMap
  return (Map.lookup jn j)

getFirstToProcess :: PDState (Maybe Juggler)
getFirstToProcess = do
  jList <- Lens.use toProcess
  return (maybeHead jList)
    where maybeHead [] = Nothing
          maybeHead (x:_) = Just x

getJuggFromCirc :: CircuitName -> PDState [Juggler]
getJuggFromCirc cn = do
  circMap <- Lens.use circuits
  let jList = Map.lookup cn circMap
  case Map.lookup cn circMap of
   Nothing -> error $ "getJuggFromCirc: " ++ show cn
   Just js -> return js

getCircuitLen :: CircuitName -> PDState (Maybe Int)
getCircuitLen cn =
  if null cn
  then return Nothing
  else do jList <- getJuggFromCirc cn
          return (Just (length jList))

addJuggler :: CircuitName -> Juggler -> PDState ()
addJuggler cn j = do
  circuits.at cn %= fmap (j<|)
  toProcess %= tail

removeLowJuggler :: CircuitName -> PDState Juggler
removeLowJuggler cn = do
  jList <- getJuggFromCirc cn
  let sList = sort jList
  circuits.at cn .= return (tail sList)
  return $ Lens.over jCircDP tail (head sList)

assignJuggler :: PDState ()
assignJuggler = do
  mJ <- getFirstToProcess
  case mJ of
   Nothing -> return ()
   Just j -> do
     let cn = j^.jCircDP._head._1
     addJuggler cn j
     mLen <- getCircuitLen cn
     case mLen of
      Nothing -> lost %= (j:)
      Just len -> do
        s <- Lens.use size
        when (len > s) $
          removeLowJuggler cn >>= \oldJ -> toProcess %= (oldJ:)
     assignJuggler

assignAllJugglers :: PDState ()
assignAllJugglers = do
  assignJuggler
  cMap <- Lens.use circuits
  s <- Lens.use size
  let c = Map.keys $ Map.filter (\l -> length l < s) cMap
  l <- Lens.use lost
  assignLostJugglers c l

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
