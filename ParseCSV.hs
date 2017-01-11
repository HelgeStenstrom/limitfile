module ParseCSV (csvFile, Cell)
where

import System.IO

import Text.Parsec (many1, digit, char, spaces, anyChar, eof, between, many, noneOf, parseTest, (<|>), try, string, (<?>), sepBy, endBy, Parsec )
-- import Text.Parsec.Char
import Text.Parsec.String (Parser)


-- En quotedChar finns inuti ett citat. De flesta tecken är sig
-- själva, men " skrivs "". Så "" inuti ett citat ska returnera ".
-- I koden skrivs " som \".
quotedChar :: Parser Char
quotedChar = 
   noneOf "\"" 
   <|> try (string "\"\"" >> return '"')


-- En quotedCell börjar och slutar med ", och använder quotedChar för
-- sitt innehåll, istället för char.
quotedCell :: Parser String
quotedCell = do
  char '"'
  content  <- many quotedChar
  char '"' <?> "quote at end of cell"
  return content

-- en rawCell är textinnehållet från en cell i ett CSV-rutnät. 
rawCell :: Parser String
rawCell = quotedCell <|> many (noneOf ";\n\r")
-- Så småningom vill jag ha en cell, som innehåller även cellnummer.
-- Den blir en Parser Cell.

-- en Line består av många celler, som var och en är en String.
type Line = [String]

line :: Parser Line 
line = sepBy rawCell (char ';')
-- line = do { cells <- sepBy rawCell (char ';') ; return $ [head cells]}

type Header = String
data Table = Table Header [Line]

type Cell = String
type ParserCell = Parsec Cell ()

parseHeader :: Parser Header
parseHeader = between (char '[')  (char ']') 
                 (many (noneOf "]")) <* eof
-- För att parseHeader ska vara användbar, måste den tillämpas där
-- parsad sträng tar slut efter önskad text. Det skiljer den från
-- quotedCell.

-- Men det är så jag vill ha det. Jag vill inte blanda in citerade
-- celler när jag parsar header-celler. Likaså vill jag slippa sådant
-- när jag parsar formler.


headerCell :: Parser String
headerCell = do
  char '['
  content  <- many $ noneOf "]"
  char ']' <?> "] at end of cell"
  return content


-- headerLine :: Parsec Line ()  [String]
-- headerLine = do
--   head <- parseHeader
--   return [head]

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"


csvFile :: Parser [[String]]
csvFile = endBy line eol


readAndParse :: String -> IO ()
readAndParse fname = do
  inh <- openFile fname ReadMode
  alltext <- hGetContents inh
  -- print $   alltext
  parseTest csvFile alltext
  hClose inh
  putStrLn "hej"

demo = do
  parseTest quotedChar "\"\""                             -- returns '"'
  parseTest quotedCell  "\"cell with \"\" in it\"  "      -- returns "cell with \" in it"
  parseTest rawCell "one;two;three"                       -- returns "one"
  parseTest line "one;two;three;\"four\";five"            -- returns ["one","two","three","four","five"]
  parseTest parseHeader "[head]"                          -- returns "head"
  parseTest headerCell "[head]"                           -- returns "head"
  -- parseTest headerLine "[head]; two; three"               -- fails. 
  parseTest eol "\rNextLine"                              -- returns "\r"
  parseTest csvFile "one;two\rThree; four\r"              -- returns [["one","two"],["Three"," four"]



-- Main är bara till för att kunna testa min kod på ett enkelt
-- sätt. När den här filen är färdig som biblioteksrutin, behövs inte
-- Main mer.

main = readAndParse "BLK_147_0446_B66A_PF52.csv"
