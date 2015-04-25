{-# LANGUAGE TemplateHaskell #-}
import qualified Control.Lens as Lens
import Control.Lens ((^.),(%=),(.=),(<|),at,_1,_2,_head,_tail)
import Control.Monad (when)
import qualified Control.Monad.State as St
import qualified Data.Either as Either
import Data.List (intercalate,sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>),(<?>))
import Text.Printf (printf)

type CircuitName = String
type JugglerName = String
type Parser = Parsec.Parsec String ()
type FileLine = Either.Either Circuit JugglerRaw
type PDState = St.State ProcessData
type CircuitDP = (CircuitName, Int)

data Skill = Skill { _h :: Int, _e :: Int, _p :: Int }
data Circuit = Circuit { _cName :: CircuitName, _cSkill :: Skill }
data JugglerRaw = JugglerRaw
                  { _jrName :: JugglerName
                  , _jrSkill :: Skill
                  , _jrPref :: [CircuitName]
                  }
data Juggler = Juggler
               { _jName :: JugglerName
               , _jSkill :: Skill
               , _jCircDP :: [CircuitDP]
               }

instance Show (Skill) where
  show (Skill h e p) = printf "<%d %d %d>" h e p

instance Show (Circuit) where
  show (Circuit cn sk) = cn ++ " " ++ show sk

instance Show (JugglerRaw) where
  show (JugglerRaw jn sk cns) = unwords [jn, show sk, show cns]

instance Show (Juggler) where
  show (Juggler jn _ dps) = jn ++ " " ++ unwords m
    where m = map (\(x,y) -> x ++ ":" ++ show y) dps

data ProcessData = ProcessData
                   { _circMap :: Map.Map CircuitName Circuit
                   , _juggMap :: Map.Map JugglerName Juggler
                   , _circuits :: Map.Map CircuitName [Juggler]
                   , _size :: Int
                   , _toProcess :: [Juggler]
                   , _lost :: [Juggler]
                   } deriving (Show)

-- Use TH calls to create our lenses
Lens.makeLenses ''Skill
Lens.makeLenses ''Circuit
Lens.makeLenses ''JugglerRaw
Lens.makeLenses ''Juggler
Lens.makeLenses ''ProcessData

instance Eq (Juggler) where
  (==) x y = getDP x == getDP y

instance Ord (Juggler) where
  (<=) x y = getDP x <= getDP y

getDP :: Juggler -> Int
getDP j = case _jCircDP j of
           [] -> -1
           ((_,p):_) -> p

-- Allow us to calculate dot products of anything that has skill
-- We use lenses so we need to wait until after the makeLenses
class DPCalc a where
  getSkill :: (DPCalc a) => a -> Skill
  dotProduct :: (DPCalc a, DPCalc b) => a -> b -> Int
  dotProduct u v = a*x + b*y + c*z
    where Skill a b c = getSkill u
          Skill x y z = getSkill v

instance DPCalc (JugglerRaw) where getSkill = _jrSkill
instance DPCalc (Circuit) where getSkill = _cSkill
instance DPCalc (Skill) where getSkill = id

parseSkill :: Parser Skill
parseSkill = do
  Parsec.string " H:"
  h <- Parsec.many1 Parsec.digit
  Parsec.string " E:"
  e <- Parsec.many1 Parsec.digit
  Parsec.string " P:"
  p <- Parsec.many1 Parsec.digit
  return (Skill (str2Int h) (str2Int e) (str2Int p))
  where str2Int s = read s :: Int

parseCircuit :: Parser Circuit
parseCircuit = do
  Parsec.string "C "
  name <- Parsec.many1 Parsec.alphaNum
  sk <- parseSkill
  return (Circuit name sk)

parseCL :: Parser [CircuitName]
parseCL = Parsec.many1 Parsec.alphaNum `Parsec.sepBy` Parsec.char ','

parseJuggler :: Parser JugglerRaw
parseJuggler = do
  Parsec.string "J "
  name <- Parsec.many1 Parsec.alphaNum
  sk <- parseSkill
  Parsec.space
  cl <- parseCL
  return (JugglerRaw name sk cl)

parseLine :: Parser FileLine
parseLine = do
    c <- parseCircuit
    return (Left c)
  <|> do
    j <- parseJuggler
    return (Right j)
  <|> (eol >> parseLine)

parseLines :: Parser [FileLine]
parseLines = Parsec.endBy parseLine eol

eol =   Parsec.try (Parsec.string "\n\r")
    <|> Parsec.try (Parsec.string "\r\n")
    <|> Parsec.string "\n"
    <|> Parsec.string "\r"
    <?> "end of line"

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

calcJuggDP :: Map.Map CircuitName Circuit -> JugglerRaw -> Juggler
calcJuggDP cMap jr = Juggler (jr^.jrName) (jr^.jrSkill) dps
  where dps = map (\c -> (c^.cName, dotProduct jr c)) cList
        cList = mapMaybe (`Map.lookup` cMap) (jr^.jrPref)

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

foo :: PDState [String]
foo = do
  assignAllJugglers
  c <- Lens.use circMap
  convertToLine [] (Map.elems c)

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
  let cn = c^.cName
  jugglers <- getJuggFromCirc cn
  let line = cn ++ ' ' : intercalate "," (map show jugglers)
  convertToLine (line : acc) cs

main = processFile "jugglefest.txt" "jugglefest.out.txt"
-- grep ^C1970 jugglefest.out.txt | sed 's/J/\n/g' | awk '!/^C/ {print $1}' |  awk '{total=total+$1} END{print total}'
