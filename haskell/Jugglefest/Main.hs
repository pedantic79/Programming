module Main where
import qualified Control.Monad.Identity as Id
import qualified Control.Monad.State as St
import qualified Control.Monad.Trans.Either as EitherT
import qualified Data.Either as Either
import qualified Data.Map.Strict as Map
import qualified Text.Parsec as Parsec

import Control.Monad.Trans.Class (lift)
import Control.Arrow ((&&&))
import Data.Maybe (mapMaybe)

import Parse
import State
import Types

calcJuggDP :: Map.Map CircuitName Circuit -> JugglerRaw -> Juggler
calcJuggDP cMap jr = Juggler (jrName jr) (jrSkill jr) dps
  where dps = fmap (cName &&& dotProduct jr) cList
        cList = mapMaybe (`Map.lookup` cMap) (jrPref jr)

processFile :: FilePath -> FilePath -> EitherT.EitherT String IO ()
processFile f o = do
  c <- lift $ readFile f
  case Parsec.parse parseLines f c of
    Left l  -> EitherT.left . show $ l
    Right r -> lift . writeFile o . unlines . evaluate $ r

mapMkMap :: Ord k => (a -> (k, v)) -> [a] ->  Map.Map k v
mapMkMap = (Map.fromList .) . fmap

mkProcessData :: [Circuit] -> [JugglerRaw] -> ProcessData
mkProcessData c jr = ProcessData cMap jMap oMap s jugg []
  where
    lenNum :: [a] -> Double
    lenNum = fromIntegral . length
    s = ceiling (lenNum jr / lenNum c)
    jugg = fmap (calcJuggDP cMap) jr
    cMap = mapMkMap (cName &&& id) c
    jMap = mapMkMap (jName &&& id) jugg
    oMap = mapMkMap (\x -> (cName x, [])) c

evaluate :: [FileLine] -> [String]
evaluate f = Id.runIdentity . St.evalStateT assign $
             mkProcessData circuits rawJugglers
  where (circuits, rawJugglers) = Either.partitionEithers f

main :: IO()
main = do
  et <- EitherT.runEitherT $ processFile "jugglefest.txt" "jugglefest.out.txt"
  case et of
    Left l -> putStrLn l
    Right _ -> return ()
-- grep ^C1970 jugglefest.out.txt | sed 's/J/\n/g' | awk '!/^C/ {print $1}' |  awk '{total=total+$1} END{print total}'
