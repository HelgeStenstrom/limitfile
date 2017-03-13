module Excel  (excelColumns, excelColumn, excel2Number) --, eColumn)

where

excelColumns :: [String]
excelColumns = map excelColumn [0..]


excelColumn :: Int -> String
excelColumn n | (n<0) = "unknown"
              | otherwise =       (!!) (sequence =<< (tail $ iterate ( ['A'..'Z']:) [] ) ) n

excel2Number :: String -> Int
excel2Number s =  (flip foldl 0 $ \accum ch -> accum * 26 + fromEnum ch - fromEnum 'A' + 1) s - 1

type APair = (Int, Int)
type ExcelColumn = String
type ExcelRow = Int

newtype NumberedCell a  = NumberedCell ((ExcelColumn, ExcelRow), a)

type StringCell = NumberedCell String
type IntCell = NumberedCell Int

