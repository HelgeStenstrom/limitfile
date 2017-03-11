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
   , tokenTests
   -- , topLevelTests
    ]
    
tableNameTests = TestList [
     tableNameWithinBrackets
   , tableNameWithinBracketsEvenWithBlanksBefore
    ]

tokenTests = TestList [
     utraBecomesCarrierStandardSameCase
   , eutraBecomesCarrierStandardSameCase
   , utraMixedCase
   , eutraMixedCase
   , asteriskMeansAnyStandard
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


-- =================

utraBecomesCarrierStandardSameCase =
   parse carrierStandard "" "UTRA"  ~?= Right Utra
eutraBecomesCarrierStandardSameCase =
   parse carrierStandard "" "EUTRA"  ~?= Right Eutra

utraMixedCase =
   parse carrierStandard "" "UtRa"  ~?= Right Utra
eutraMixedCase =
   parse carrierStandard "" "EuTrA"  ~?= Right Eutra

asteriskMeansAnyStandard =
  parse carrierStandard "" "*"  ~?= Right Any

-- =================
-- =================

parsingEmptyFileReturnsEmptyList =
   1 ~?= 2


