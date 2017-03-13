-- This file implements Csv format. There are many that do. This is an attempt.

module Csv
where

import Text.Parsec.String (Parser)
import Text.Parsec ((<|>), char, many, noneOf, eof)
import HelpFunctions

-- ======= CSV-fil till celler =========
csvCell :: Parser String
csvCell = do
        whitespace
        s <- many $ noneOf "; \n\r"
        -- char ';' -- <|> char '\n'
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
        ss <- csvCells
        char '\n'
        return ss

csvLines :: Parser [[String]]
csvLines = do
         (many csvLine <* eof)
-- ======= CSV-fil till celler =========
