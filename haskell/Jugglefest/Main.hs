module Main where
import qualified Control.Monad.State as St
import qualified Data.Either as Either
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Text.Parsec as Parsec
import Control.Lens ((^.))
import Types
import State
import Parse

calcJuggDP :: Map.Map CircuitName Circuit -> JugglerRaw -> Juggler
calcJuggDP cMap jr = Juggler (jr^.jrName) (jr^.jrSkill) dps
  where dps = map (\c -> (c^.cName, dotProduct jr c)) cList
        cList = mapMaybe (`Map.lookup` cMap) (jr^.jrPref)

processFile :: FilePath -> FilePath -> IO ()
processFile f o = do
  c <- readFile f
  case Parsec.parse parseLines f c of
   Left e -> putStrLn "Error parsing input:" >> print e
   Right r -> writeFile o (unlines $ evaluate r)

mkProcessData :: [Circuit] -> [JugglerRaw] -> ProcessData
mkProcessData c jr = ProcessData cMap jMap oMap s jugg []
  where
    lenNum = fromIntegral . length
    mapMkMap fn = Map.fromList . map fn
    s = ceiling (lenNum jr / lenNum c)
    jugg = map (calcJuggDP cMap) jr
    cMap = mapMkMap (\x -> (x^.cName, x)) c
    jMap = mapMkMap (\j -> (j^.jName, j)) jugg
    oMap = mapMkMap (\x -> (x^.cName, [])) c

evaluate :: [FileLine] -> [String]
evaluate f = St.evalState assign $
             mkProcessData circuits rawJugglers
  where (circuits, rawJugglers) = Either.partitionEithers f


main = processFile "jugglefest.txt" "jugglefest.out.txt"
-- grep ^C1970 jugglefest.out.txt | sed 's/J/\n/g' | awk '!/^C/ {print $1}' |  awk '{total=total+$1} END{print total}'
