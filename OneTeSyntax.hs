-- This file implements a few syntaxes found in CSV files.

module OneTeSyntax
where


import Data.Char (toLower, toUpper)
-- import Data.Text (dropWhileEnd, dropWhile)
-- import Text.Parsec.Token
import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Debug.Trace (trace)
import Text.Parsec (parseTest, char, noneOf, oneOf, sepBy, sepBy1, many1, many, eof)
import Text.Parsec.Char (anyChar, string, digit)
import Text.Parsec.Combinator (manyTill)
import Text.Parsec.Prim (ParsecT, getParserState, parse, stateInput, try, (<|>), (<?>) )
import Text.Parsec.String (Parser)
-- import Text.Parsec.Error -- .ParseError
-- import Text.Parsec.Pos 
import qualified Test.HUnit as H

import FunctionsAndTypesForParsing
import HelpFunctions


trim :: String -> String
trim = (dropWhile (\c -> c== ' ')) . reverse .  (dropWhile (\c -> c== ' ')) . reverse

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

-- blanks :: Parser ()
-- blanks = void $ many $ char ' '



-- ======= Datatyper inom limitfiler ====

-- Match the lowercase or uppercase form of 'c'
caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

-- Match the string 's', accepting either lowercase or uppercase form of each character 
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

data CarrierStandard = Utra | Eutra | Any deriving (Show, Eq)

carrierStandard :: Parser CarrierStandard
carrierStandard = do
   try (caseInsensitiveString "UTRA" >> pure Utra) <|> (caseInsensitiveString "EUTRA" >> pure Eutra)
   <|> (char '*' >> pure Any)


data CarrierBW = Bandwidths [Int] | AnyBW deriving (Show, Eq, Read)

carrierBW :: Parser CarrierBW
carrierBW = do
     cb <- try (cBW2)  <|> ( cBW11) 
     return cb


cBW11 :: Parser CarrierBW
cBW11 = do
      ns <- pipeSepNumList
      return $ Bandwidths ns

cBW2 :: Parser CarrierBW
cBW2 = do
     whitespace
     cbw <- try (char '*' >> pure AnyBW ) 
     -- eof
     return cbw


-- ======= Datatyper inom limitfiler ====
-- ======= Syntax inom celler ==========

pipeSepList :: Parser [String]
pipeSepList  = do
             whitespace
             s <- sepBy1 (many1 $ noneOf "|")  (char '|')
             return $ map trim s



pipeSepNumList :: Parser [Int]
pipeSepNumList = do
               ns <- many1 pipeSepIntItem
               return $ map read ns

pipe :: Parser Char
pipe = lexeme $ char '|'

pipeSep1 ::  Parser [String]
pipeSep1 = sepBy (many1 $ noneOf "|") pipe

pipeSepIntItem :: Parser String
pipeSepIntItem = do
   whitespace
   n <- many1 digit
   void $ many $ char ' '
   void (char '|') <|> eof
   return n

pipeSepFloatItem :: Parser String
pipeSepFloatItem = do
   whitespace
   n1 <- many1 (digit) 
   n2 <- try decimalPart
   void $ many $ char ' '
   void (char '|') <|> eof
   return  (n1++n2)

decimalPart :: Parser String
decimalPart = do
  c <- char '.'
  ds <- many1 digit
  return (c:ds)

-- ======= Syntax inom celler ==========

tableName :: Parser String
tableName = do
  whitespace
  char '['
  s <- many $ noneOf "]"
  char ']'
  whitespace
  eof
  return s

