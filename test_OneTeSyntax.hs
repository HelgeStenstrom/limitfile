module Main
where
import Test.HUnit
import OneTeSyntax
import Text.Parsec (parse)
import Text.Parsec.Error
import FunctionsAndTypesForParsing

main = do runTestTT tests

tests = TestList [
   tableNameTests
   , pipeListTests
   , tokenTests
   -- , topLevelTests
    ]
    


tableNameTests = TestList [
     tableNameWithinBracketsWithExtraChars
   , tableNameWithinBracketsEvenWithBlanksBefore
    ]


pipeListTests = TestList [
      splitStringsByPipe
    , splittingLonelyStringByPipeReturnsString
    , whiteSpaceIsTrimmed
    , whiteSpaceIsTrimmedForAllWords
    , pipeListMustContainOneToken
    , pipeListMustContainOneToken1
    , pipeListMalformed
    , pipeSepIntItemTestWithSingleNumber
    , pipeSepIntItemTestWithPipe
    , pipeSepFloatItemTestWithSingleNumber
     ]


tokenTests = TestList [
     utraBecomesCarrierStandardSameCase
   , eutraBecomesCarrierStandardSameCase
   , utraMixedCase
   , eutraMixedCase
   , asteriskMeansAnyStandard
   , carrierBandwidthMHzIsListOfInt
   , carrierBandwidthMHzIsAnyBW
    ]

topLevelTests = TestList [
    parsingEmptyFileReturnsEmptyList
    ]

-- splitStringsByPipe = 
--     parse pipeSepList "" "abc|def|ghi" ~?= Right ["abc", "def", "ghi"]

splitStringsByPipe = 
    parseWithLeftOver pipeSepList "abc|def|ghi" ~?= Right (["abc", "def", "ghi"], "")

splittingLonelyStringByPipeReturnsString =
    parseWithLeftOver pipeSepList  "abc" ~?= Right (["abc"], "")

whiteSpaceIsTrimmed =
   parseWithLeftOver pipeSepList  "  x  " ~?= Right (["x"], "")

whiteSpaceIsTrimmedForAllWords =
   parseWithLeftOver pipeSepList  "  x  | y   |z  " ~?= Right (["x", "y", "z"], "")

pipeListMustContainOneToken =
   isLeft (parse pipeSepList "pipeListMustContainOneToken" "  ") ~?= True

pipeListMustContainOneToken1 =
   TestCase $ assertBool "fel" (isLeft (parse pipeSepList "pipeListMustContainOneToken" "  "))

pipeListMalformed =
   TestCase $ assertBool "Ska inte acceptera flera tal utan pipe emellan"
              (isLeft (parse pipeSepNumList "" " 1  1"))

pipeSepIntItemTestWithSingleNumber =
    parseWithLeftOver pipeSepIntItem   " 123   " ~?= Right ("123", "")

pipeSepIntItemTestWithPipe = 
    parseWithLeftOver pipeSepIntItem   " 123  | 34 " ~?= Right ("123", " 34 ")

pipeSepFloatItemTestWithSingleNumber =
    parseWithLeftOver pipeSepFloatItem   " 1.23  | 34 " ~?= Right ("1.23", " 34 ")


--------------------------------------------------------------------

tableNameWithinBracketsWithExtraChars = 
   isLeft (parseWithLeftOver tableName  "[my title]...") ~?= True

tableNameWithinBracketsEvenWithBlanksBefore =
   parseWithLeftOver tableName  "  [my title]" ~?= Right ("my title", "")


-- =================

utraBecomesCarrierStandardSameCase =
   parseWithLeftOver carrierStandard  "UTRA"  ~?= Right (Utra, "")
eutraBecomesCarrierStandardSameCase =
   parse carrierStandard "" "EUTRA"  ~?= Right Eutra

utraMixedCase =
   parse carrierStandard "" "UtRa"  ~?= Right Utra
eutraMixedCase =
   parse carrierStandard "" "EuTrA"  ~?= Right Eutra

asteriskMeansAnyStandard =
  parse carrierStandard "asteriskMeansAnyStandard" "*"  ~?= Right Any

-- =================
carrierBandwidthMHzIsListOfInt = 
  parse carrierBW "carrierBandwidthMHzIsListOfInt" "5|10|15|20"  ~?= Right (Bandwidths [5, 10, 15, 20])
  
carrierBandwidthMHzIsAnyBW = 
  parse carrierBW "carrierBandwidthMHzIsAnyBW" "*"  ~?= Right AnyBW
  
-- =================

parsingEmptyFileReturnsEmptyList =
   1 ~?= 2

-- willFail_1 =
   










-- -- ============ Helpers
isLeft :: (Either l r) -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

