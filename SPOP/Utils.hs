module Utils 
 where
 
import System.IO
 
data InputData = InputData { rows:: String
                           , cols:: String
                           , pairs:: String
                           }
                           
data IntPair = IntPair { first:: Int
                       , second:: Int} deriving (Show) 
                           
printInputData :: InputData -> IO ()
printInputData (InputData a b c) = do 
 putStrLn (a ++ b ++ c)  
 return() 
 
printPairData :: IntPair -> IO ()
printPairData (IntPair a b) = do 
 print (a)  
 print (b)
 return() 
 
arrayToPair :: [Int] -> [IntPair]
arrayToPair [] = []
arrayToPair (x1:x2:xs) = (IntPair x1 x2) : (arrayToPair xs)
 
parseToInt :: String -> [Int]
parseToInt s = do
 let rw = [ x | x <- s, not (x `elem` ",[]") ]
 ret <- map read $ words rw :: [Int]
 return ret
 
parseToPairs :: String -> [IntPair]
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