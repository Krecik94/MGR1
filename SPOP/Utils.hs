module Utils 
 where
 
import System.IO
 
data InputData = InputData { rows:: String
                           , cols:: String
                           , pairs:: String
                           }     
                           
printInputData :: InputData -> IO ()
printInputData (InputData a b c) = do 
 putStrLn (a ++ b ++ c)  
 return() 
 
make2dArray :: Int -> Int -> [(Int, Int)] -> Int -> [[Char]]
make2dArray 0 _ _ _ = []
make2dArray rowNum colNum houses currRow = (addRow colNum houses currRow 0) : (make2dArray (rowNum-1) colNum houses (currRow+1))

addRow :: Int -> [(Int, Int)] -> Int -> Int ->[Char] 
addRow 0 _ _ _ = [];
addRow colNum houses currRow currCol | ((currRow, currCol) `elem` houses) = "D" ++ (addRow (colNum-1) houses currRow (currCol+1))
                                     | otherwise = "W" ++ (addRow (colNum-1) houses currRow (currCol+1))
 
arrayToPair :: [Int] -> [(Int, Int)]
arrayToPair [] = []
arrayToPair (x1:x2:xs) = (x1, x2) : (arrayToPair xs)
 
parseToInt :: String -> [Int]
parseToInt s = do
 let rw = [ x | x <- s, not (x `elem` ",[]") ]
 ret <- map read $ words rw :: [Int]
 return ret
 
parseToPairs :: String -> [(Int, Int)]
parseToPairs s = do
 let rw = [ x | x <- s, not (x `elem` ",[]()") ]
 let intArray = map read $ words rw :: [Int]
 ret <- arrayToPair intArray
 return ret

getInputData :: IO InputData
getInputData = do
 fname <- getLine
 handle <- openFile fname ReadMode
 rows <- hGetLine handle
 --rows <- map (read . (:"")) "12345" :: [Int]
 cols <- hGetLine handle
 pairs <- hGetLine handle
 hClose handle
 return (InputData rows cols pairs)