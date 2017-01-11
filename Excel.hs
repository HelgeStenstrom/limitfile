module Excel (excelColumns, excelColumn) where

excelColumns :: [String]
excelColumns = map eColumn [0..]

eColumn :: Int -> String
eColumn col | (0 <= col) && (col < 26) = lowEColumn col
            | (col >= 26) = (eColumn $ (div col  26 ) -1 ) ++ eColumn  ( mod col  26) 
            | otherwise = "unknown"

lowEColumns = map (\c -> [c])  ['A'..'Z']

lowEColumn :: Int -> String
lowEColumn n = lowEColumns !! n

excelColumn :: Int -> String
excelColumn n | (n<0) = "unknown"
              | otherwise =       (!!) (sequence =<< (tail $ iterate ( ['A'..'Z']:) [] ) ) n
-- excelColumn = (!!) (sequence =<< (tail $ iterate ( ['A'..'Z']:) [] ) )

p1 n = eColumn n == excelColumn n


excel2Number :: String -> Int
excel2Number = flip foldl 0 $ \accum ch -> accum * 26 + fromEnum ch - fromEnum 'A' + 1

-- misslyckas för n < 0
p2 n = n == ( (-1) + (excel2Number $ eColumn n))

type APair = (Int, Int)
type ExcelColumn = String
type ExcelRow = Int

newtype NumberedCell a  = NumberedCell ((ExcelColumn, ExcelRow), a)

type StringCell = NumberedCell String
type IntCell = NumberedCell Int

