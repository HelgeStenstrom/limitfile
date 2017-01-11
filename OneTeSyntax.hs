-- This file implements a few syntaxes found in CSV files.

module OneTeSyntax ()
where


-- import Data.Char (isSpace)
-- import Data.Text (dropWhileEnd, dropWhile)
-- import Text.Parsec.Token
import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Debug.Trace (trace)
import Text.Parsec (parseTest, char, noneOf, oneOf, sepBy, many1, many)
import Text.Parsec.Char (anyChar)
import Text.Parsec.Combinator (manyTill)
import Text.Parsec.Prim (ParsecT, getParserState, stateInput)
import Text.Parsec.String (Parser)

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
  parseTest betweenBraces "{12345}"
  