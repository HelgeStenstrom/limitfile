module Main
where
import Test.HUnit
import Csv
import Text.Parsec (parse)

main = do runTestTT tests

tests = TestList [
     csvCellTests
   , csvLineTests
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

cellWithNoWhiteSpace =
   parse csvCell "" "cell;" ~?= Right "cell"

cellWithWhiteSpace =
   parse csvCell "cellWithWhiteSpace" "  cell ; " ~?= Right "cell"

cellsEndedBySemiColon =
   parse csvCells "cellsEndedBySemiColon" "  a ; b  ;" ~?= Right ["a", "b", ""]



emptyLineReturnsEmptyList = 
   parse csvLine "emptyLineReturnsEmptyList" "" ~?= Right [""]



