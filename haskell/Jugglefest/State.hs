module State (assign) where
import qualified Control.Lens as Lens
import Control.Lens ((^.),(<|),(%=),(.=),_1,_head,at)
import Control.Monad (liftM,liftM2,forM, mzero,when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.List (intercalate,sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes,listToMaybe,fromMaybe)
import Data.Monoid (mappend)
import Types
import Lens

{-# ANN module "HLint: ignore Use fmap" #-}

getJuggler :: JugglerName -> PDState (Maybe Juggler)
getJuggler jn = Lens.use $ juggMap.at jn

getCircuits :: CircuitName -> PDState (Maybe [Juggler])
getCircuits cn = Lens.use $ circuits.at cn

getFirstToProcess :: PDState (Maybe Juggler)
getFirstToProcess = liftM listToMaybe $ Lens.use toProcess

getJuggFromCirc :: CircuitName -> PDState [Juggler]
getJuggFromCirc cn = liftM (fromMaybe err) $ getCircuits cn
  where
   err = error $ "getJuggFromCirc: " `mappend` show cn

getCircuitLen :: CircuitName -> MaybeT PDState Int
getCircuitLen (CircuitName []) = mzero
getCircuitLen cn = liftM length . lift . getJuggFromCirc $ cn

addJuggler :: CircuitName -> Juggler -> PDState ()
addJuggler cn j = do
  circuits.at cn %= liftM (j<|) -- (<|) = cons
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
    Just j  -> assignFirstJuggler j

assignFirstJuggler :: Juggler -> PDState ()
assignFirstJuggler j = do
  let cn = j^.jCircDP._head._1
  addJuggler cn j
  mLen <- runMaybeT . getCircuitLen $ cn
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
assignLostJugglers  _ [ ] = return ()
assignLostJugglers [ ] _ = return ()
assignLostJugglers cAll@(c:cs) (j:js) = do
  cLen <- runMaybeT . getCircuitLen $ c
  s <- Lens.use size
  case cLen of
   Nothing -> error $ "assignLostJugglers: " `mappend` show cLen
   Just cl -> do
     circuits.at c %= liftM (j<|)
     if cl + 1 < s
       then assignLostJugglers cAll js
       else assignLostJugglers cs js

convertToLine :: [Circuit] -> PDState [String]
convertToLine cs = forM cs go
  where
    go c = do
      let cn = cName c
      jugglers <- getJuggFromCirc cn
      pristine <- mapM (getJuggler . jName) jugglers
      let commas = intercalate "," (map show . catMaybes $ pristine)
      let line = show cn `mappend` (' ' : commas)
      return line

assign :: PDState [String]
assign = do
  assignAllJugglers
  c <- Lens.use circMap
  convertToLine (Map.elems c)
