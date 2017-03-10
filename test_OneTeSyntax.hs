module Main
where
import Test.HUnit
import OneTeSyntax
import Text.Parsec (parse)

main = do runTestTT tests

tests = TestList [
     splitStringsByPipe
   , splittingLonelyStringByPipeReturnsString
   , whiteSpaceIsTrimmed
   , whiteSpaceIsTrimmedForAllWords
   , tableNameWithinBrackets
   , tableNameWithinBracketsEvenWithBlanksBefore
   , tableNameTests
   , csvCellTests
   -- , csvLineTests
   -- , topLevelTests
    ]
tableNameTests = TestList [
     tableNameWithinBrackets
   , tableNameWithinBracketsEvenWithBlanksBefore
    ]

csvCellTests =  TestList [
    cellWithNoWhiteSpace
  , cellWithWhiteSpace
  , cellsEndedBySemiColon
    ]

csvLineTests =  TestList [
    emptyLineReturnsEmptyList
  -- , splitOnSemicolonUntilEOL
    ]

topLevelTests = TestList [
    parsingEmptyFileReturnsEmptyList
    ]

splitStringsByPipe = 
    parse pipeSepList "" "abc|def|ghi" ~?= Right ["abc", "def", "ghi"]

splittingLonelyStringByPipeReturnsString =
    parse pipeSepList "" "abc" ~?= Right ["abc"]

whiteSpaceIsTrimmed =
   parse pipeSepList "" "  x  " ~?= Right ["x"]

whiteSpaceIsTrimmedForAllWords =
   parse pipeSepList "" "  x  | y   |z  " ~?= Right ["x", "y", "z"]

tableNameWithinBrackets = 
   parse tableName "" "[my title]..." ~?= Right "my title"
-- Jag vet inte hur jag vill göra med oparsad text. Innan jag vet det,
-- kan jag inte testa.

tableNameWithinBracketsEvenWithBlanksBefore =
   parse tableName "" "  [my title]..." ~?= Right "my title"

cellWithNoWhiteSpace =
   parse csvCell "" "cell;" ~?= Right "cell"

cellWithWhiteSpace =
   parse csvCell "cellWithWhiteSpace" "  cell ; " ~?= Right "cell"

cellsEndedBySemiColon =
   parse csvCells "cellsEndedBySemiColon" "  a ; b  ;" ~?= Right ["a", "b", ""]



emptyLineReturnsEmptyList = 
   parse csvLine "emptyLineReturnsEmptyList" "" ~?= Right [""]


parsingEmptyFileReturnsEmptyList =
   1 ~?= 2
   