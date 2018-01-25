import Utils
import System.IO
import Data.Typeable

run = do
 putStrLn "Input filename"
 info <- getInputData
 printInputData info
 let row = parseToInt (rows info)
 let col = parseToInt (cols info)
 let pair = parseToPairs (pairs info)
 let board = make2dArray (length row) (length col) pair row col 0
 print board
 testPrint board row
 return ()
 
main = do
 putStrLn "Welcome"
 run
 