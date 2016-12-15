import Data.List
main = readLn >>= \n -> mapM_ construct [1..n]
construct _ = do
	a <- getLine
	b <- getLine
	putStrLn $ hasCommon (nub a) b

hasCommon src xs
	| any (\c -> any (c==) src) xs = "YES"
	| otherwise = "NO"
