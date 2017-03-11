-- This file implements a few syntaxes found in CSV files.

module OneTeSyntax
where


import Data.Char (toLower, toUpper)
-- import Data.Text (dropWhileEnd, dropWhile)
-- import Text.Parsec.Token
import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Debug.Trace (trace)
import Text.Parsec (parseTest, char, noneOf, oneOf, sepBy, many1, many, eof)
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



-- ======= Datatyper inom limitfiler ====
-- ======= Syntax inom celler ==========

pipeSepList :: Parser [String]
pipeSepList  = do
             s <- sepBy (many1 $ noneOf "|")  (char '|')
             return $ map trim s
pipe :: Parser Char
pipe = lexeme $ char '|'

pipeSep1 ::  Parser [String]
pipeSep1 = sepBy (many1 $ noneOf "|") pipe

-- ======= Syntax inom celler ==========

-- ======= celler till tabeller med namn ====
-- ======= celler till tabeller med namn ====
-- ======= Övrigt ======================
-- ======= Övrigt ======================


-- From https://www.evernote.com/shard/s4/nl/295093/64d0cfd5-39dd-4f38-a2c2-8dc3d7bc7561/
-- https://www.reddit.com/r/haskelltil/comments/3el2d6/a_handy_function_for_debugging_in_parsec_by_what/
println msg = trace (show msg) $ return ()

seeNext :: Int -> ParsecT String u Identity ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  println out

betweenBraces = char '{' >> manyTill (seeNext 10 >> anyChar) (char '}')
-- end of above article

tableName :: Parser String
tableName = do
  whitespace
  char '['
  s <- many $ noneOf "]"
  char ']'
  return s

demo = do
  parseTest pipeSepList "aaa | b|c|d|  e e e"
  -- parseTest betweenBraces "{12345}"
  parseTest pipeSep1  "aaa | b|c|  d  |  e e e   "
  parseTest (lexeme $ char '|') "|  123 "


demo2 = do
    -- parseWithLeftOver (lexeme $ char '|') "|  123 "
    1

-- escape :: Either Text.Parsec.Error.ParseError (Char, String) -> (Char, String)
-- escape Right Text.Parsec.Error.ParseError r = ('r', "right")
-- -- escape Just  Text.Parsec.Error.ParseError (c, s) = (c, s)
-- escape Left _ = ('x', "wrong")

--
-- *OneTeSyntax> parseWithLeftOver (lexeme $ char '|') "|  123 "
-- Right ('|',"123 ")
-- *OneTeSyntax> :t it
-- it :: Either Text.Parsec.Error.ParseError (Char, String)
-- *OneTeSyntax> 


-- ===============================
-- Unit testing

-- whitespace
wsExamples :: [(String, ())]
wsExamples = [(" ", ())
            ,("\t", ())
            ,("   ", ())
            ,("    \t", ())
            ]
wsTests = mkTests (\_ -> whitespace)  wsExamples


-- char
charExamples :: [(String, Char)]
charExamples = [("a", 'a')
               ,("     a", 'a')]
charTests :: [H.Test]
charTests = mkTests char charExamples

-- digit
digitExamples :: [(String, String)]
digitExamples =  [("123", "123")
                 ,("",    "")
                 ,(" 123","123")]
-- digitTests = mkTests (many digit) digitExamples
digitTests = mkTests2 (\_ -> (many digit)) digitExamples


numberExamples :: [(String,Integer)]
numberExamples = [("1", 1)]



-- string
stringExamples :: [(String,String)]
stringExamples = [("a", "a")
                 ,("ab", "ab")
                 ,("abc", "abc")]
stringTests = mkTests string  stringExamples



mkTests :: (Show a, Eq a) => (a -> Parser a) -> [(String, a)] -> [H.Test]
mkTests p exs = map (\(src, exp) -> makeTest (p exp) (src, exp)) exs 

mkTests2 :: (Show a, Eq a) => (a -> Parser a) -> [(String, a)] -> [H.Test]
mkTests2 p exs = map (\(src, exp) -> makeTest2 (p exp) (src, exp)) exs 

makeTest :: (Eq a, Show a) => Parser a -> (String,a) -> H.Test
makeTest parser (src,expected) = H.TestLabel src $ H.TestCase $ do
    let gote = parse (whitespace *> parser <* eof) "" src
    case gote of
      Left e -> H.assertFailure $ show e
      Right got -> H.assertEqual src expected got

makeTest2 :: (Eq a, Show a) => Parser a -> (String,a) -> H.Test
makeTest2 parser (src,expected) = H.TestLabel src $ H.TestCase $ do
    let gote = parse (whitespace *> parser ) "" src
    case gote of
      Left e -> H.assertFailure $ show e
      Right got -> H.assertEqual src expected got



doTests = do
 H.runTestTT $ H.TestList $ charTests ++ stringTests ++ wsTests ++ digitTests

 
-- end Unit testing
-- ===============================
