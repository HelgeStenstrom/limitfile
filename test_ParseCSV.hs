-- module Main where
import ParseCSV (csvFile)
import Text.Parsec (parseTest) -- many1, digit, char, spaces, anyChar, eof, between, many, noneOf, parseTest, (<|>), try, string, (<?>), sepBy, endBy, Parsec )



-- cell1 = Cell 2 3 "A cell in row 2, column 3"


demo = do
  -- parseTest quotedChar "\"\""                             -- returns '"'
  -- parseTest quotedCell  "\"cell with \"\" in it\"  "      -- returns "cell with \" in it"
  -- parseTest rawCell "one;two;three"                       -- returns "one"
  -- parseTest line "one;two;three;\"four\";five"            -- returns ["one","two","three","four","five"]
  -- parseTest parseHeader "[head]"                          -- returns "head"
  -- parseTest headerCell "[head]"                           -- returns "head"
  -- -- parseTest headerLine "[head]; two; three"               -- fails. 
  -- parseTest eol "\rNextLine"                              -- returns "\r"
  parseTest csvFile "one;two\rThree; four\r"              -- returns [["one","two"],["Three"," four"]
