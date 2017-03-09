module Main
where
import Test.HUnit
import OneTeSyntax
import Text.Parsec (parse)

main = do runTestTT tests

tests = TestList [
    findWordBeforePipe
    ]

findWordBeforePipe = 
    parse pipeSepList "" "abc|def" ~?= Right ["abc", "def"]
