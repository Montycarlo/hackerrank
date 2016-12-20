
main = do
	n <- readLn
	parts <- partition n
	putStrLn . unwords $ map show parts

partition :: Int -> IO [Int]
partition n = do
	l_str <- getLine
	let 
		ls = map read $ words l_str
		p = ls !! 0
		left = filter (p>) ls
		equal = filter (p==) ls
		right = filter(p<) ls
	return $ left++equal++right

