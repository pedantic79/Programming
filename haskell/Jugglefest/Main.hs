module Main where
import qualified Control.Monad.Identity as Id
import qualified Control.Monad.State as St
import qualified Control.Monad.Trans.Either as ET
import qualified Data.Either as E
import qualified Data.Map.Strict as Map
import qualified Text.Parsec as Parsec

import Control.Monad.Trans.Class (lift)
import Control.Arrow ((&&&))
import Data.Maybe (mapMaybe)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPrint, stderr)

import Parse
import State
import Types

calcJuggDP :: Map.Map CircuitName Circuit -> JugglerRaw -> Juggler
calcJuggDP cMap jr = Juggler (jrName jr) (jrSkill jr) dps
  where
    dps = fmap (cName &&& dotProduct jr) cList
    cList = mapMaybe (`Map.lookup` cMap) (jrPref jr)

processFile :: FilePath -> ET.EitherT Parsec.ParseError IO [FileLine]
processFile f = do
  c <- lift $ readFile f
  ET.hoistEither $ Parsec.parse parseLines f c

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

runProcessData :: PDState a -> ProcessData -> a
runProcessData = (Id.runIdentity .) . St.evalStateT

evaluate :: [FileLine] -> [String]
evaluate f = runProcessData assign $ mkProcessData circuits rawJugglers
  where
    (circuits, rawJugglers) = E.partitionEithers f

main :: IO ()
main = do
  et <- ET.runEitherT $ processFile "jugglefest.txt"
  case et of
    Left  l -> hPrint stderr l >> exitWith (ExitFailure 3)
    Right r -> writeFile "jugglefest.out.txt" . unlines . evaluate $ r

-- grep ^C1970 jugglefest.out.txt | sed 's/J/\n/g' | awk '!/^C/ {print $1}' |  awk '{total=total+$1} END{print total}'
