module Utils 
 where

import Control.Monad.Trans
import Control.Monad.Trans.List
 
import System.IO
import Data.List
 
data InputData = InputData { rows:: String
                           , cols:: String
                           , pairs:: String
                           }     
                           
printInputData :: InputData -> IO ()
printInputData (InputData a b c) = do 
 putStrLn (a ++ b ++ c)  
 return() 
 
make2dArray :: Int -> Int -> [(Int, Int)] -> [Int] -> [Int] -> Int -> [[Char]]
make2dArray 0 _ _ _ _ _ = []
make2dArray rowNum colNum houses rowGas colGas currRow = (addRow colNum houses rowGas colGas currRow 0) : (make2dArray (rowNum-1) colNum houses rowGas colGas (currRow+1))

addRow :: Int -> [(Int, Int)] -> [Int] -> [Int] -> Int -> Int ->[Char] 
addRow 0 _ _ _ _ _ = [];
addRow colNum houses rowGas colGas currRow currCol | ((currRow, currCol) `elem` houses)              = "D" ++ (addRow (colNum-1) houses rowGas colGas currRow (currCol+1))
                                                   | (rowGas!!currRow) == 0 || (colGas!!currCol) == 0
                                                     || ( not ((currRow, currCol+1) `elem` houses)
                                                         && not ((currRow, currCol-1) `elem` houses)
                                                         && not ((currRow+1, currCol) `elem` houses)
                                                         && not ((currRow-1, currCol) `elem` houses))= "X" ++ (addRow (colNum-1) houses rowGas colGas currRow (currCol+1))
                                                   | otherwise                                       = "W" ++ (addRow (colNum-1) houses rowGas colGas currRow (currCol+1))
 
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

value input_list = do
 x <- ListT (return input_list)
 lift (putStr . show $ x)
 
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



 
testPrint table row = do
 putStrLn "TEST"
 print row
 putStr " "
 --runListT (value row)
 