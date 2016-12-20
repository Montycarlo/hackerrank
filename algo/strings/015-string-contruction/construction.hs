import Data.List
main = readLn >>= \n -> mapM_ construct [1..n]
construct _ = getLine >>= putStrLn . show . length . nub
