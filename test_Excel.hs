module Main
where

import Test.HUnit
import Excel


main = do runTestTT tests

tests = TestList [
      firstColumnIsA
    , aBitOfAlphabet  
      ]


firstColumnIsA =
  excelColumn 0 ~?= "A"

aBitOfAlphabet =
  concat (map excelColumn [0..25])  ~?= "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


