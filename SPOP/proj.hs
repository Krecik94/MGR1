import Utils
import System.IO
import Data.Typeable

run = do
 info <- getInputData
 printInputData info
 let row = parseToInt (rows info)
 let col = parseToInt (cols info)
 let pair = parseToPairs (pairs info)
 let board = make2dArray (length row) (length col) pair 0
 print board
 return ()
 