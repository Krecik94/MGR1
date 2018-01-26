module Utils 
 where

import Control.Monad.Trans
import Control.Monad.Trans.List
 
import System.IO
import Data.List
 
-- structure to parse data into
data InputData = InputData { rows:: String
                           , cols:: String
                           , pairs:: String
                           }     
-- test function                     
printInputData :: InputData -> IO ()
printInputData (InputData a b c) = do 
 putStrLn (a ++ b ++ c)  
 return() 
-- creating board
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
                                                   | otherwise                                       = "_" ++ (addRow (colNum-1) houses rowGas colGas currRow (currCol+1))

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


-- main function calculating next iteration of possible tank placements
testFunc :: [[Char]] -> [Int] -> [Int] -> [[Char]]
testFunc board row col = do
 let newBoard = checkRow board row
 let newBoardA = transpose (checkRow (transpose newBoard) col)
 let newBoardB = iterateOverRowsToFill newBoardA newBoardA 0
 newBoardC <- iterateOverRows newBoardB newBoardB 0
 return newBoardC
 
-- helper functions to iterate over rows
checkRow :: [[Char]] -> [Int] -> [[Char]]
checkRow [] _ = []
checkRow (line:board) (row:rows) | (length (filter (\x  -> x == 'Z') line))== row = (iterateOnElementFill line) : (checkRow board rows)
                                 | (length (filter (\x  -> x == '_'|| x == 'Z') line))== row = (iterateOnElement line) : (checkRow board rows)
                                 | otherwise = line : (checkRow board rows)

iterateOverRows :: [[Char]] -> [[Char]] -> Int -> [[Char]]
iterateOverRows _ [] _ = []
iterateOverRows board  (row:boardA) currRow = (checkHouse board row currRow 0):(iterateOverRows board boardA (currRow+1))

iterateOverRowsToFill :: [[Char]] -> [[Char]] -> Int -> [[Char]]
iterateOverRowsToFill _ [] _ = []
iterateOverRowsToFill board  (row:boardA) currRow = (checkForBlanks board row currRow 0):(iterateOverRowsToFill board boardA (currRow+1))

-- function placing X around Z
checkForBlanks _ [] _ _ = []
checkForBlanks board  (row:boardA) currRow currCol | (row=='_' &&(currRow > 0 )&&(board!!(currRow-1))!!currCol == 'Z') 
                                                            = 'X':(checkForBlanks board boardA currRow (currCol+1))
                                               | (row=='_' &&(((currRow +1) < (length board)))&&(board!!(currRow+1))!!currCol == 'Z') 
                                                            = 'X':(checkForBlanks board boardA currRow (currCol+1))
                                               | (row=='_' &&(currCol > 0 )&&(board!!currRow)!!(currCol-1) == 'Z') 
                                                            = 'X':(checkForBlanks board boardA currRow (currCol+1))
                                               | (row=='_' &&(((currCol +1) < (length (board!!0))))&&(board!!currRow)!!(currCol+1) == 'Z') 
                                                            = 'X':(checkForBlanks board boardA currRow (currCol+1))             
                                               | otherwise = row:(checkForBlanks board boardA currRow (currCol+1))


-- function placing Z when it's only possible placement for D
checkHouse _ [] _ _ = []
checkHouse board  (row:boardA) currRow currCol | (row=='_' &&(currRow > 0 )&&(board!!(currRow-1))!!currCol == 'D') 
                                                            && (((currRow-1)<=0||(((board!!(currRow-2))!!currCol /= '_') && ((board!!(currRow-2))!!currCol /= 'Z'))) 
                                                                && (currCol<=0||(((board!!(currRow-1))!!(currCol-1) /= '_') && ((board!!(currRow-1))!!(currCol-1) /= 'Z'))) 
                                                                && (((currCol+1)>=(length (board!!0)))||(((board!!(currRow-1))!!(currCol+1) /= '_') && ((board!!(currRow-1))!!(currCol+1) /= 'Z'))))
                                                            = 'Z':(checkHouse board boardA currRow (currCol+1))
                                               | (row=='_' &&(((currRow +1) < (length board)))&&(board!!(currRow+1))!!currCol == 'D') 
                                                            && (((((currRow +2) >= (length board)))||(((board!!(currRow+2))!!currCol /= '_') && ((board!!(currRow+2))!!currCol /= 'Z'))) 
                                                                && (currCol<=0||(((board!!(currRow+1))!!(currCol-1) /= '_') && ((board!!(currRow+1))!!(currCol-1) /= 'Z'))) 
                                                                && (((currCol+1)>=(length (board!!0)))||(((board!!(currRow+1))!!(currCol+1) /= '_') && ((board!!(currRow+1))!!(currCol+1) /= 'Z')))) 
                                                            = 'Z':(checkHouse board boardA currRow (currCol+1))
                                               | (row=='_' &&(currCol > 0 )&&(board!!currRow)!!(currCol-1) == 'D') 
                                                            && (((currCol-1)<=0||(((board!!currRow)!!(currCol-2) /= '_') && ((board!!currRow)!!(currCol-2) /= 'Z'))) 
                                                                && (currRow<=0||(((board!!(currRow-1))!!(currCol-1) /= '_') && ((board!!(currRow-1))!!(currCol-1) /= 'Z'))) 
                                                                && (((currRow+1)>=(length board))||(((board!!(currRow+1))!!(currCol-1) /= '_') && ((board!!(currRow+1))!!(currCol-1) /= 'Z'))))
                                                            = 'Z':(checkHouse board boardA currRow (currCol+1))
                                               | (row=='_' &&(((currCol +1) < (length (board!!0))))&&(board!!currRow)!!(currCol+1) == 'D') 
                                                            && (((currCol+2)>=(length (board!!0))||(((board!!currRow)!!(currCol+2) /= '_') && ((board!!currRow)!!(currCol+2) /= 'Z'))) 
                                                                && (currRow<=0||(((board!!(currRow-1))!!(currCol+1) /= '_') && ((board!!(currRow-1))!!(currCol+1) /= 'Z'))) 
                                                                && (((currRow+1)>=(length board))||(((board!!(currRow+1))!!(currCol+1) /= '_') && ((board!!(currRow+1))!!(currCol+1) /= 'Z'))))
                                                            = 'Z':(checkHouse board boardA currRow (currCol+1))             
                                               | otherwise = row:(checkHouse board boardA currRow (currCol+1))

-- functions iterating over elements
iterateOnElementFill :: [Char] -> [Char]
iterateOnElementFill [] = []
iterateOnElementFill (l:line) | l == '_' = 'X':(iterateOnElementFill line)
                          | otherwise = l:(iterateOnElementFill line)

iterateOnElement :: [Char] -> [Char]
iterateOnElement [] = []
iterateOnElement (l:line) | l == '_' = 'Z':(iterateOnElement line)
                          | otherwise = l:(iterateOnElement line)


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

 
-- helper functions to print the board on screen
valueOther [] [] = do
 return ()
 
valueOther (x:xs) (y:ys) = do
 --x <- ListT (return ys)
 --y <- ListT (return input_board)
 lift (putStr . show $ y)
 lift (putStr x)
 lift (putStrLn "")
 valueOther xs ys
 
value input_list = do
 x <- ListT (return input_list)
 lift (putStr . show $ x)
 
-- function to print board on screen
testPrint board col row = do
 putStrLn ""
 runListT (value col)
 putStrLn ""
 runListT (valueOther board row)
 