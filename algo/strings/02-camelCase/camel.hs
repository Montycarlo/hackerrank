import Data.Char
main = getLine >>= \n -> putStrLn $ show ((+) 1 $ length $ filter isUpper n) 
