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
   , topLevelTests
   , caseInsStringTests
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
    , pipeSepFloatFailOnMultiPoint1
    , pipeSepFloatFailOnMultiPoint2
     ]


tokenTests = TestList [
     utraBecomesCarrierStandardSameCase
   , eutraBecomesCarrierStandardSameCase
   , utraMixedCase
   , eutraMixedCase
   , failingCarrierStandard
   , asteriskMeansAnyStandard
   , carrierBandwidthMHzIsListOfInt
   , carrierBandwidthMHzIsAnyBW
    ]

topLevelTests = TestList [
    -- parsingEmptyFileReturnsEmptyList
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

pipeSepFloatFailOnMultiPoint1 = 
  isLeft (parse pipeSepFloatItem  "pipeSepFloatFailOnMultiPoint1" "1..23") ~?= True

pipeSepFloatFailOnMultiPoint2 = 
  isLeft (parse pipeSepFloatItem  "pipeSepFloatFailOnMultiPoint2" "1.23.45") ~?= True


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

failingCarrierStandard =
   isLeft (parse carrierStandard "" "xxx" ) ~?= True

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

   

caseInsStringTests = TestList [
   caseInsString1
 , caseInsString2
 , caseInsString3
 , caseInsString4
 , caseInsStringFail
 , caseInsStringFail1
   ]


caseInsString1 =
   parseWithLeftOver  (caseInsensitiveString "a")  "aTheRest" ~?= Right ("a", "TheRest")
caseInsString2 =
   parseWithLeftOver  (caseInsensitiveString "a")  "ATheRest" ~?= Right ("A", "TheRest")
caseInsString3 =
   parseWithLeftOver  (caseInsensitiveString "A")  "aTheRest" ~?= Right ("a", "TheRest")
caseInsString4 =
   parseWithLeftOver  (caseInsensitiveString "A")  "ATheRest" ~?= Right ("A", "TheRest")
caseInsStringFail =
  isLeft (parseWithLeftOver (caseInsensitiveString "A")  "No match" ) ~?= True

caseInsStringFail1 =
 (fmap show $ fmap errorPos $  swapLR $ parseWithLeftOver (caseInsensitiveString "A")  "No match")  ~?= (Right "(line 1, column 1)")





-- -- ============ Helpers
isLeft :: (Either l r) -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

-- errorPosOf :: Either ParseError b -> Either Text.Parsec.Pos.SourcePos b
errorPosOf (Left l) = Left (errorPos l)
errorPosOf (Right r)   = Right r

errorMessOf (Left l) = Left (errorMessages l)
errorMessOf (Right r)   = Right r

swapLR :: (Either l r) -> Either r l
swapLR (Left l) = Right l
swapLR (Right r) = Left r

