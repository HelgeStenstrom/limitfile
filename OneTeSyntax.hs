-- This file implements a few syntaxes found in CSV files.

module OneTeSyntax ()
where


-- import Data.Char (isSpace)
-- import Data.Text (dropWhileEnd, dropWhile)
-- import Text.Parsec.Token
import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Debug.Trace (trace)
import Text.Parsec (parseTest, char, noneOf, oneOf, sepBy, many1, many, eof)
import Text.Parsec.Char (anyChar, string, digit)
import Text.Parsec.Combinator (manyTill)
import Text.Parsec.Prim -- (ParsecT, getParserState, stateInput)
import Text.Parsec.String (Parser)
import Text.Parsec.Error -- .ParseError
import Text.Parsec.Pos 
import qualified Test.HUnit as H

import FunctionsAndTypesForParsing


whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

pipeSepList :: Parser [String]
pipeSepList  = sepBy (many1 $ noneOf "|")  (char '|')

trim :: String -> String
trim = (dropWhile (\c -> c== ' ')) . reverse .  (dropWhile (\c -> c== ' ')) . reverse

blanks :: Parser ()
blanks = void $ many $ char ' '

pipe :: Parser Char
pipe = lexeme $ char '|'

pipeSep1 ::  Parser [String]
pipeSep1 = sepBy (many1 $ noneOf "|") pipe

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



mkTests
  :: (Show a, Eq a) => (a -> Parser a) -> [(String, a)] -> [H.Test]
mkTests p exs = map (\(src, exp) -> makeTest (p exp) (src, exp)) exs 

mkTests2
  :: (Show a, Eq a) => (a -> Parser a) -> [(String, a)] -> [H.Test]
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
