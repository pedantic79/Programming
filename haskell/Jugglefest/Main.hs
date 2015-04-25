module Main where
import qualified Control.Monad.State as St
import qualified Data.Either as Either
import qualified Data.Map.Strict as Map
import qualified Text.Parsec as Parsec
import Control.Lens ((^.))
import Types
import State
import Parse

processFile :: FilePath -> FilePath -> IO ()
processFile f out = do
  c <- readFile f
  case Parsec.parse parseLines f c of
   Left e -> do putStrLn "Error parsing input:"
                print e
   Right r -> writeFile out (unlines $ doStuff r)

doStuff :: [FileLine] -> [String]
doStuff f = St.evalState foo pd
  where
    pd = ProcessData cMap (mkJMap jugg) (mkOutM circ) s jugg []
    (circ, juggRaw) = Either.partitionEithers f
    cMap = mkCMap circ
    jugg = map (calcJuggDP cMap) juggRaw
    lenNum = fromIntegral . length
    s = ceiling (lenNum jugg / lenNum circ)
    mkCMap = Map.fromList . map (\c -> (c^.cName, c))
    mkOutM = Map.fromList . map (\c -> (c^.cName, []))
    mkJMap = Map.fromList . map (\j -> (j^.jName, j))

main = processFile "jugglefest.txt" "jugglefest.out.txt"
-- grep ^C1970 jugglefest.out.txt | sed 's/J/\n/g' | awk '!/^C/ {print $1}' |  awk '{total=total+$1} END{print total}'
