{-# LANGUAGE TemplateHaskell #-}
import qualified Control.Lens as Lens
import Control.Lens ((^.))
import qualified Control.Monad.State as St
import Data.Either (Either, lefts, rights)
import Data.List (intercalate)
import qualified Data.Map.Lazy as Map
import Data.Maybe (mapMaybe)
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>),(<?>))
import Text.Printf (printf)

type CircuitName = String
type JugglerName = String
type FileLine = Either Circuit Juggler

data Skill = Skill { _h :: Int, _e :: Int, _p :: Int }
data Circuit = Circuit { _cName :: CircuitName, _cSkill :: Skill }
data Juggler = Juggler
               { _jName :: JugglerName
               , _jSkill :: Skill
               , _jPref :: [CircuitName]
               }

instance Show (Skill) where
  show (Skill h e p) = printf "<%d %d %d>" h e p

instance Show (Circuit) where
  show (Circuit cn sk) = cn ++ " " ++ show sk

instance Show (Juggler) where
  show (Juggler jn sk cns) = jn ++ " " ++ show sk ++ " " ++ showCNList cns
    where showCNList n = "[" ++ intercalate "," n ++ "]"


data ProcessData = ProcessData
                   { _circMap :: Map.Map CircuitName Circuit
                   , _juggMap :: Map.Map JugglerName Juggler
                   , _circuits :: Map.Map CircuitName [JugglerName]
                   , _size :: Int
                   , _toProcess :: [JugglerName]
                   } deriving (Show)

type PDState = St.State ProcessData

-- Use TH calls to create our lenses
Lens.makeLenses ''Skill
Lens.makeLenses ''Circuit
Lens.makeLenses ''Juggler
Lens.makeLenses ''ProcessData

str2Int s = read s :: Int

parseSkill :: Parsec.Parsec String () Skill
parseSkill = do
  Parsec.string " H:"
  h <- Parsec.many1 Parsec.digit
  Parsec.string " E:"
  e <- Parsec.many1 Parsec.digit
  Parsec.string " P:"
  p <- Parsec.many1 Parsec.digit
  return (Skill (str2Int h) (str2Int e) (str2Int p))

parseCircuit :: Parsec.Parsec String () Circuit
parseCircuit = do
  Parsec.string "C "
  name <- Parsec.many1 Parsec.alphaNum
  sk <- parseSkill
  return (Circuit name sk)

parseCL :: Parsec.Parsec String () [CircuitName]
parseCL = Parsec.many1 Parsec.alphaNum `Parsec.sepBy` Parsec.char ','

parseJuggler :: Parsec.Parsec String () Juggler
parseJuggler = do
  Parsec.string "J "
  name <- Parsec.many1 Parsec.alphaNum
  sk <- parseSkill
  Parsec.space
  cl <- parseCL
  return (Juggler name sk cl)

parseLine :: Parsec.Parsec String () FileLine
parseLine = do
    c <- parseCircuit
    return (Left c)
  <|> do
    j <- parseJuggler
    return (Right j)
  <|> (eol >> parseLine)

parseLines :: Parsec.Parsec String () [FileLine]
parseLines = Parsec.endBy parseLine eol

eol =   Parsec.try (Parsec.string "\n\r")
    <|> Parsec.try (Parsec.string "\r\n")
    <|> Parsec.string "\n"
    <|> Parsec.string "\r"
    <?> "end of line"

processFile :: FilePath -> IO ()
processFile f = do
  c <- readFile f
  case Parsec.parse parseLines f c of
   Left e -> do putStrLn "Error parsing input:"
                print e
--   Right r -> mapM_ print $ doStuff r
   Right r -> print $ doStuff r

dotProductSk :: Skill -> Skill -> Int
dotProductSk s t = s^.h*t^.h  +  s^.e*t^.e  +  s^.p*t^.p

dotProductCrJg :: Circuit -> Juggler -> Int
dotProductCrJg c j = dotProductSk (c^.cSkill) (j^.jSkill)

getJugglers :: [JugglerName] -> PDState [Maybe Juggler]
getJugglers = mapM getJuggler

getJuggler :: JugglerName -> PDState (Maybe Juggler)
getJuggler jn = do
  j <- Lens.use juggMap
  return (Map.lookup jn j)

foo :: PDState [Juggler]
foo = do
  names <- Lens.use toProcess
  l <- getJugglers names
  return (mapMaybe id l)

doStuff f = St.evalState foo pd
  where
    pd = ProcessData (mkCMap circ) (mkJMap jugg) (mkOutM circ) s toP
    circ = lefts f
    jugg = rights f
    lenNum = fromIntegral . length
    s = ceiling (lenNum jugg / lenNum circ)
    toP = map (^.jName) jugg
    mkCMap = Map.fromList . map (\c -> (c^.cName, c))
    mkOutM = Map.fromList . map (\c -> (c^.cName, []))
    mkJMap = Map.fromList . map (\j -> (j^.jName, j))
    
