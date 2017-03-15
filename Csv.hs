-- This file implements Csv format. There are many that do. This is an attempt.

module Csv (csvCell, csvCells, csvLine, csvLines)
where

import Text.Parsec.String (Parser)
import Text.Parsec ((<|>), char, many, noneOf, eof)
import HelpFunctions
import Control.Monad

import Excel

-- ======= CSV-fil till celler =========
eol :: Parser Char
eol = char '\n'

csvCell :: Parser String
csvCell = do
        whitespace
        s <- many $ noneOf "; \n\r"
        whitespace
        return s

csvCells :: Parser [String]
csvCells = do
        first <- csvCell
        next <- remainingCells
        return (first:next)

remainingCells :: Parser [String]
remainingCells = do
               (char ';' >> csvCells) <|> (return [] )

csvLine :: Parser [String]
csvLine = do
        csvCells <* eol


csvLines :: Parser [[String]]
csvLines = do
         (many csvLine <* eof)
-- ======= CSV-fil till celler =========


-- ======= celler med cellnamn =========
-- ======= celler med cellnamn =========
