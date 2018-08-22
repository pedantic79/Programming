module Main where
import qualified Control.Monad.Identity as Id
import qualified Control.Monad.State as St
import qualified Data.Either as E
import qualified Data.Map.Strict as Map
import qualified Text.Parsec as Parsec

import Control.Monad.Trans.Class (lift)
import Control.Arrow ((&&&))
import Data.Function (on)
import Data.Maybe (mapMaybe)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPrint, stderr)

import Parse
import State
import Types

-- (f &&& g) = \x -> (f x, g x)

calcJuggDP :: Map.Map CircuitName Circuit -> JugglerRaw -> Juggler
calcJuggDP cMap jr = Juggler (jrName jr) (jrSkill jr) dps
  where
    dps = fmap (cName &&& dotProduct jr) cList
    cList = mapMaybe (`Map.lookup` cMap) (jrPref jr)

processFile :: FilePath -> IO (Either Parsec.ParseError [FileLine])
processFile f = do
  c <- readFile f
  return $ Parsec.parse parseLines f c

mapMkMap :: Ord k => (a -> (k, v)) -> [a] ->  Map.Map k v
mapMkMap = (Map.fromList .) . fmap

intDiv :: Int -> Int -> Double
intDiv = (/) `on` fromIntegral

mkProcessData :: [Circuit] -> [JugglerRaw] -> ProcessData
mkProcessData c jr = ProcessData cMap jMap oMap s jugg []
  where
    s = ceiling $ intDiv (length jr) (length c)
    jugg = fmap (calcJuggDP cMap) jr
    cMap = mapMkMap (cName &&& id) c
    jMap = mapMkMap (jName &&& id) jugg
    oMap = mapMkMap (cName &&& const []) c

runProcessData :: PDState a -> ProcessData -> a
runProcessData = (Id.runIdentity .) . St.evalStateT

evaluate :: [FileLine] -> [String]
evaluate = runProcessData assign . uncurry mkProcessData . E.partitionEithers

main :: IO ()
main = do
  et <- processFile "jugglefest.txt"
  case et of
    Left  l -> hPrint stderr l >> exitWith (ExitFailure 3)
    Right r -> writeFile "jugglefest.out.txt" . unlines . evaluate $ r

-- grep ^C1970 jugglefest.out.txt | sed 's/J/\n/g' | awk '!/^C/ {print $1}' |  awk '{total=total+$1} END{print total}'
