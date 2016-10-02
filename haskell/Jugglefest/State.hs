module State (assign) where
import qualified Control.Lens as Lens
import qualified Data.Map.Strict as Map

import Control.Lens ((^.),(<|),(%=),(.=),_1,_head,at)
import Control.Monad (liftM, forM_, mzero, when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.List (intercalate,sort)
import Data.Maybe (catMaybes,listToMaybe,fromMaybe)
import Data.Monoid ((<>))

import Lens
import Types

{-# ANN module "HLint: ignore Use fmap" #-}

getJuggler :: JugglerName -> PDState (Maybe Juggler)
getJuggler jn = Lens.use $ juggMap.at jn

getCircuit :: CircuitName -> PDState (Maybe [Juggler])
getCircuit cn = Lens.use $ circuits.at cn

getFirstToProcess :: PDState (Maybe Juggler)
getFirstToProcess = listToMaybe <$> Lens.use toProcess

getJuggFromCirc :: CircuitName -> PDState [Juggler]
getJuggFromCirc cn = fromMaybe err <$> getCircuit cn
  where
   err = error $ "getJuggFromCirc: [" <> show cn <> "]"

getCircuitLen :: CircuitName -> MaybeT PDState Int
getCircuitLen (CircuitName []) = mzero
getCircuitLen cn = liftM length . lift . getJuggFromCirc $ cn

addJuggler :: CircuitName -> Juggler -> PDState ()
addJuggler cn j = do
  circuits.at cn %= liftM (j<|) -- (<|) = cons
  toProcess %= tail

removeLowJuggler :: CircuitName -> PDState Juggler
removeLowJuggler cn = do
  (s:xs) <- sort <$> getJuggFromCirc cn
  circuits.at cn .= pure xs
  return $ Lens.over jCircDP tail s

assignJuggler :: PDState ()
assignJuggler = do
  mJ <- getFirstToProcess
  forM_ mJ assignFirstJuggler

assignFirstJuggler :: Juggler -> PDState ()
assignFirstJuggler j = do
  let cn = j^.jCircDP._head._1
  addJuggler cn j
  mLen <- runMaybeT . getCircuitLen $ cn
  case mLen of
    -- append j to lost list, because there are no circuits
    Nothing -> lost %= (j<|)
    Just len -> do
      s <- Lens.use size
      when (len > s) $ do
        oldJ <- removeLowJuggler cn
        toProcess %= (oldJ<|)
  assignJuggler

assignAllJugglers :: PDState ()
assignAllJugglers = do
  assignJuggler
  c <- fn <$> Lens.use size <*> Lens.use circuits
  l <- Lens.use lost
  assignLostJugglers c l
  where
    fn s = Map.keys . Map.filter (\l -> length l < s)

assignLostJugglers :: [CircuitName] -> [Juggler] -> PDState ()
assignLostJugglers  _ [ ] = return ()
assignLostJugglers [ ] _  = return ()
assignLostJugglers cAll@(c:cs) (j:js) = do
  cLen <- runMaybeT . getCircuitLen $ c
  s <- Lens.use size
  case cLen of
    Nothing -> error $ "assignLostJugglers: [" <> show cLen <> "]"
    Just cl -> do
      circuits.at c %= liftM (j<|)
      if cl + 1 < s
        then assignLostJugglers cAll js
        else assignLostJugglers cs js

convertToLine :: [Circuit] -> PDState [String]
convertToLine = mapM go
  where
    go c = do
      let cn = cName c
      jugglers <- getJuggFromCirc cn
      pristine <- mapM (getJuggler . jName) jugglers
      let commas = intercalate "," (map show . catMaybes $ pristine)
      let line = show cn <> (' ' : commas)
      return line

assign :: PDState [String]
assign = do
  assignAllJugglers
  convertToLine =<< Map.elems <$> Lens.use circMap
