import Utils
import System.IO
import Data.Typeable

run = do
 info <- getInputData
 printInputData info
 let row = parseToInt (rows info)
 let col = parseToInt (cols info)
 print row
 return ()
 