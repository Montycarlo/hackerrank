
main = do
	n <- getLine
	ar_str <- getLine
	partition $ map read $ words ar_str

partition :: [Int] -> IO [Int]
partition [] = return []
partition [x] = return [x]
partition ls = do
	let 
		p = ls !! 0
		left = filter (p>) ls
		equal = filter (p==) ls
		right = filter(p<) ls
	left2 <- partition left
	right2 <- partition right
	let sorted = left2++equal++right2
	putStrLn . unwords $ map show sorted
	return sorted

