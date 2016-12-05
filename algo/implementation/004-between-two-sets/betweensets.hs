
readInts :: IO [Int]
readInts = do
	l <- getLine
	return $ map read $ words l

main = getLine >> do
	as <- readInts
	bs <- readInts
	let 
		min_ = foldl max 1 as
		max_ = foldl min 100 bs
	putStrLn . show . length $ [x | x <- [min_..max_], 
			all (\i -> x `rem` i == 0) as && all (\i -> i `rem` x == 0) bs]
