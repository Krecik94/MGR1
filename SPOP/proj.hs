import Utils
import System.IO
import Data.Typeable

run = do
 putStrLn "Input filename"
-- reading data
 info <- getInputData
 printInputData info
-- parsing data
 let row = parseToInt (rows info)
 let col = parseToInt (cols info)
 let pair = parseToPairs (pairs info)
 let board = make2dArray (length row) (length col) pair row col 0
 print board
-- start iterating
 testPrint board col row
 let newerBoard = testFunc board row col
 let newestBoard = testFunc newerBoard row col
 let newestBoardA = testFunc newestBoard row col
-- print results.
-- prints all generated tables for easier debugging
 testPrint newerBoard col row
 testPrint newestBoard col row
 testPrint newestBoardA col row
 return ()
 
-- main function, launches when program is started
main = do
 putStrLn "Welcome"
 run
 