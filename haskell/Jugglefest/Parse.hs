module Parse (parseLines) where

import qualified Text.Parsec as Parsec
import Control.Applicative (liftA, (<$>), (*>))
import Text.Parsec ((<|>),(<?>))
import Types

{-# ANN module "HLint: ignore Use fmap" #-}

type Parser = Parsec.Parsec String ()

parseSkill :: Parser Skill
parseSkill = do
  _  <- Parsec.string " H:"
  h' <- Parsec.many1 Parsec.digit
  _  <- Parsec.string " E:"
  e' <- Parsec.many1 Parsec.digit
  _  <- Parsec.string " P:"
  p' <- Parsec.many1 Parsec.digit
  return $ Skill (str2Int h') (str2Int e') (str2Int p')
  where
    str2Int s = read s :: Int

parseCircuit :: Parser Circuit
parseCircuit = do
  _    <- Parsec.string "C "
  name <- Parsec.many1 Parsec.alphaNum
  sk   <- parseSkill
  return $ Circuit (CircuitName name) sk

-- parseCircuit :: Parser Circuit
-- parseCircuit = Circuit
--   <$> (Parsec.string "C " *> liftA CircuitName (Parsec.many1 Parsec.alphaNum))
--   <*> parseSkill

parseCircList :: Parser [CircuitName]
parseCircList = liftA CircuitName <$> Parsec.many1 Parsec.alphaNum `Parsec.sepBy` Parsec.char ','

parseJuggler :: Parser JugglerRaw
parseJuggler = do
  _    <- Parsec.string "J "
  name <- Parsec.many1 Parsec.alphaNum
  sk   <- parseSkill
  _    <- Parsec.space
  cl   <- parseCircList
  return $ JugglerRaw (JugglerName name) sk cl

parseLine :: Parser FileLine
parseLine = Left  <$> parseCircuit
        <|> Right <$> parseJuggler
        <|> (eol *> parseLine)

parseLines :: Parser [FileLine]
parseLines = Parsec.endBy parseLine eol

eol :: Parser String
eol =   Parsec.try (Parsec.string "\n\r")
    <|> Parsec.try (Parsec.string "\r\n")
    <|> Parsec.string "\n"
    <|> Parsec.string "\r"
    <?> "end of line"
