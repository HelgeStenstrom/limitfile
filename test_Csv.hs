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
  -- , oneLineNoEOL
  , oneLineseEmpty
  , twoLines
  , twoLinesText
  -- , twoLinesTextNoEOL  -- It's OK to require EOL at end of file.
    ]

cellWithNoWhiteSpace =
   parse csvCell "cellWithNoWhiteSpace" "cell;" ~?= Right "cell"

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

oneLineNoEOL =
   parse csvLine "oneLineNoEOL" "a;b;c" ~?= Right ["a", "b", "c"]


oneLineseEmpty = 
   parse csvLines "oneLineseEmpty" "\n" ~?= Right [[""]]

twoLines =
   parse csvLines "twoLines" "\n\n" ~?= Right [[""],[""]]

twoLinesText =
   parse csvLines "twoLinesText" "a; b  \n\n  c  ; d    ;e \n" ~?= Right [["a", "b"],
                                                                      [""],
                                                                      ["c", "d", "e"]]

twoLinesTextNoEOL =
   parse csvLines "twoLinesTextNoEOL" "a; b  \n\n  c  ; d    ;e " ~?= Right [["a", "b"],
                                                                      [""],
                                                                      ["c", "d", "e"]]

-- ======= celler med cellnamn =========

namedCsvCellsTests = TestList [
        csvLineWithColumnNames
        ]


csvLineWithColumnNames =
  1 ~?= 1
  -- parse csvLineNamedColumn "csvLineWithColumnNames"  "a;b;c;d"  ~?= Right 1

