module Main
where

import Test.HUnit
import Excel


main = do runTestTT tests

tests = TestList [
      firstColumnIsA
    , aBitOfAlphabet
    , alphabetAgain
    , excelKolumA
    , unknownColumn
    , letterToDigitToLetter
    , digitToLetterToDigit
      ]


firstColumnIsA =
  excelColumn 0 ~?= "A"

aBitOfAlphabet =
  concat (map excelColumn [0..25])  ~?= "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


alphabetAgain =
  concat (take 26 excelColumns) ~?= "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

unknownColumn =
  excelColumn (-1)  ~?= "unknown"

excelKolumA = 
  excel2Number "A" ~?= 0


letterToDigitToLetter =
  (excelColumn $ excel2Number "G")  ~?= "G"

digitToLetterToDigit =
  map (excel2Number . excelColumn) [0..2000]  ~?= [0..2000]

