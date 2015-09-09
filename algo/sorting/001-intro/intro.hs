indexOf _ [] = -1
indexOf n (x:xs)
	| n == x = 0
	| otherwise = 1+indexOf n xs

main = do
	v <- readLn :: IO Int
	n <- readLn :: IO Int
	ns <- getLine
	putStrLn . show . indexOf v $ map (\x->read x :: Int) $ words ns

