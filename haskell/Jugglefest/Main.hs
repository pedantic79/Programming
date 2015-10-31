module Main where
import Control.Arrow ((&&&))
import qualified Control.Monad.State as St
import qualified Data.Either as Either
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Text.Parsec as Parsec
import Types
import State
import Parse

calcJuggDP :: Map.Map CircuitName Circuit -> JugglerRaw -> Juggler
calcJuggDP cMap jr = Juggler (jrName jr) (jrSkill jr) dps
  where dps = map (cName &&& dotProduct jr) cList
        cList = mapMaybe (`Map.lookup` cMap) (jrPref jr)

processFile :: FilePath -> FilePath -> IO ()
processFile f o = do
  c <- readFile f
  case Parsec.parse parseLines f c of
   Left l -> putStrLn "Error parsing input:" >> print l
   Right r -> writeFile o (unlines $ evaluate r)

mkProcessData :: [Circuit] -> [JugglerRaw] -> ProcessData
mkProcessData c jr = ProcessData cMap jMap oMap s jugg []
  where
    lenNum :: [a] -> Double
    lenNum = fromIntegral . length
    mapMkMap = (Map.fromList .) . map
    s = ceiling (lenNum jr / lenNum c)
    jugg = map (calcJuggDP cMap) jr
    cMap = mapMkMap (cName &&& id) c
    jMap = mapMkMap (jName &&& id) jugg
    oMap = mapMkMap (\x -> (cName x, [])) c

evaluate :: [FileLine] -> [String]
evaluate f = St.evalState assign $
             mkProcessData circuits rawJugglers
  where (circuits, rawJugglers) = Either.partitionEithers f

main :: IO()
main = processFile "jugglefest.txt" "jugglefest.out.txt"
-- grep ^C1970 jugglefest.out.txt | sed 's/J/\n/g' | awk '!/^C/ {print $1}' |  awk '{total=total+$1} END{print total}'
