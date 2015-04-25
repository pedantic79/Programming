module Parse (parseLines) where

import qualified Data.Either as Either
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>),(<?>))
import Types

type Parser = Parsec.Parsec String ()

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

parseCircList :: Parser [CircuitName]
parseCircList = Parsec.many1 Parsec.alphaNum `Parsec.sepBy` Parsec.char ','

parseJuggler :: Parser JugglerRaw
parseJuggler = do
  Parsec.string "J "
  name <- Parsec.many1 Parsec.alphaNum
  sk <- parseSkill
  Parsec.space
  cl <- parseCircList
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
