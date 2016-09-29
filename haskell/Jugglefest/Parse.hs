module Parse (parseLines) where

import qualified Text.Parsec as Parsec
import Control.Applicative (liftA, (<$>), (*>))
import Text.Parsec ((<|>),(<?>))
import Types

type Parser = Parsec.Parsec String ()

parseSkillSub :: String -> Parser Int
parseSkillSub s = read <$> (Parsec.string s *> Parsec.many1 Parsec.digit)

parseCircuitName :: Parser CircuitName
parseCircuitName = CircuitName <$> (Parsec.string "C " *> Parsec.many1 Parsec.alphaNum)

parseJugglerName :: Parser JugglerName
parseJugglerName = JugglerName <$> (Parsec.string "J " *> Parsec.many1 Parsec.alphaNum)

parseSkill :: Parser Skill
parseSkill = Skill
            <$> parseSkillSub " H:"
            <*> parseSkillSub " E:"
            <*> parseSkillSub " P:"

parseCircuit :: Parser Circuit
parseCircuit = Circuit <$> parseCircuitName <*> parseSkill

parseCircList :: Parser [CircuitName]
parseCircList = liftA CircuitName <$> Parsec.many1 Parsec.alphaNum `Parsec.sepBy` Parsec.char ','

parseJuggler :: Parser JugglerRaw
parseJuggler = JugglerRaw
              <$> parseJugglerName
              <*> parseSkill
              <*> (Parsec.space *> parseCircList)

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
