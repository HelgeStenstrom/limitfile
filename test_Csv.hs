module Main
where
import Test.HUnit
import Csv
import Text.Parsec (parse)

main = do runTestTT tests

tests = TestList [
     csvCellTests
   , csvLineTests
   , namedCsvCellsTests
     ]


csvCellTests =  TestList [
    cellWithNoWhiteSpace
  , cellWithWhiteSpace
  , cellsEndedBySemiColon
  , oneCellNotEndedBySemiColon
  , twoCellsNotEndedBySemiColon
    ]

csvLineTests =  TestList [
    emptyLineReturnsEmptyList
  , oneLineseEmpty
  , twoLines
  , twoLinesText
    ]

cellWithNoWhiteSpace =
   parse csvCell "" "cell;" ~?= Right "cell"

cellWithWhiteSpace =
   parse csvCell "cellWithWhiteSpace" "  cell ; " ~?= Right "cell"

cellsEndedBySemiColon =
   parse csvCells "cellsEndedBySemiColon" "  a ; b  ;" ~?= Right ["a", "b", ""]

oneCellNotEndedBySemiColon =
   parse csvCells "oneCellNotEndedBySemiColon" "  a   " ~?= Right ["a"]

twoCellsNotEndedBySemiColon =
   parse csvCells "twoCellsNotEndedBySemiColon" "  a ; b  " ~?= Right ["a", "b"]



emptyLineReturnsEmptyList = 
   parse csvLine "emptyLineReturnsEmptyList" "\n" ~?= Right [""]

oneLineseEmpty = 
   parse csvLines "twoLines" "\n" ~?= Right [[""]]

twoLines =
   parse csvLines "twoLines" "\n\n" ~?= Right [[""],[""]]

twoLinesText =
   parse csvLines "twoLines" "a; b  \n\n  c  ; d    ;e \n" ~?= Right [["a", "b"],
                                                                      [""],
                                                                      ["c", "d", "e"]]

-- ======= celler med cellnamn =========

namedCsvCellsTests = TestList []


