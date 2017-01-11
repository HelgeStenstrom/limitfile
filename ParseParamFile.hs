-- This file is intended to model the superstructure of CSV files,
-- used in OneTE, which consists of named tables.

-- A table has a name row, with the name within [Brackets],
-- a row of column headers,
-- and one or more rows of data.

-- A cell beginning with # turns that cell and following cells on the
-- same line into comments.

-- An embedded (not first character) # in a cell doesn't turn the cell
-- into a comment.

-- Usually, a file has several tables. 


module ParseParamFile 
where

import ParseCSV

import System.IO

import Text.Parsec (Parsec) -- many1, digit, char, spaces, anyChar, eof, between, many, noneOf, parseTest, (<|>), try, string, (<?>), sepBy, endBy, Parsec )
-- import Text.Parsec.Char
-- import Text.Parsec.String (Parser)

isHeaderCell :: Cell -> Bool
isHeaderCell c = (head c == '[') && (last c == ']')

isHeaderLine :: [Cell] -> Bool
isHeaderLine l = isHeaderCell $ head l

demoCsv = [["non-header", "b1", "c1"],
           ["[Header A2]", "b2", "c2"],
           ["A3", "B3", "C3"],
           ["[Header A4]", "b4", "c4"],
           ["A5", "B5", "C5"], 
           ["[Header A6]", "b6", "c6"],
           ["A7", "B7", "C7"] ]

line = 

-- parseTable :: Parsec [[Cell]] () [[Cell]]
-- parseTable = do
--   h <- 

demo = do
  print "hej"