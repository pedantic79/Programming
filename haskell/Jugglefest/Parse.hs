import Data.Either
import Data.List (intercalate)
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>),(<?>))
import Text.Printf (printf)

type CircuitName = String
type JugglerName = String
type Foo = Either Circuit Juggler

data Skill = Skill { _h :: Int, _e :: Int, _p :: Int }
data Circuit = Circuit { _cName :: CircuitName, _cSkill :: Skill }
data Juggler = Juggler { _jName :: JugglerName
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

str2Int s = read s :: Int

parseSkill :: Parsec.Parsec String () Skill
parseSkill = do
  Parsec.string "H:"
  h <- Parsec.many1 Parsec.digit
  Parsec.spaces
  Parsec.string "E:"
  e <- Parsec.many1 Parsec.digit
  Parsec.spaces
  Parsec.string "P:"
  p <- Parsec.many1 Parsec.digit
  return (Skill (str2Int h) (str2Int e) (str2Int p))

parseCircuit :: Parsec.Parsec String () Circuit
parseCircuit = do
  Parsec.char 'C'
  Parsec.spaces
  name <- Parsec.many1 Parsec.alphaNum
  Parsec.spaces
  sk <- parseSkill
  return (Circuit name sk)

parseCircuitList :: Parsec.Parsec String () [CircuitName]
parseCircuitList = do
  first <- Parsec.many1 Parsec.alphaNum
  rest <- parseCLRest
  return (first:rest)
  where parseCLRest = do
          Parsec.char ','
          parseCircuitList
          <|> return []

parseJuggler :: Parsec.Parsec String () Juggler
parseJuggler = do
  Parsec.char 'J'
  Parsec.spaces
  name <- Parsec.many1 Parsec.alphaNum
  Parsec.spaces
  sk <- parseSkill
  Parsec.spaces
  cl <- parseCircuitList
  return (Juggler name sk cl)


parseLine :: Parsec.Parsec String () Foo
parseLine = do
  c <- parseCircuit
  return (Left c)
  <|> do
    j <- parseJuggler
    return (Right j)
  <|> do
    eol
    parseLine

parseFile :: Parsec.Parsec String () [Foo]
parseFile = Parsec.endBy parseLine eol

eol = Parsec.try (Parsec.string "\n\r")
    <|> Parsec.try (Parsec.string "\r\n")
    <|> Parsec.string "\n"
    <|> Parsec.string "\r"
    <?> "end of line"

main =
    do c <- getContents
       case Parsec.parse parseFile "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> mapM_ print r

-- Parsec.parse parseSkill "(stdin)" "H:5 E:7 P:24"
-- Parsec.parse parseCircuit "(stdin)" "C C1992 H:1 E:0 P:6"
-- Parsec.parse parseCircuitList "(stdin)" "C1223,C1325,C2352"
-- Parsec.parse parseJuggler "(stdin)"
--   "J J10000 H:8 E:8 P:2 C786,C896,C986,C1586,C1954,C1848,C326,C301,C443,C185"
