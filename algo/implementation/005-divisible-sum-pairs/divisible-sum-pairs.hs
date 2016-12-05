
readInts :: IO [Int]
readInts = getLine >>= \l ->
	return $ map read $ words l

countDivs :: Int -> [Int] -> Int -> Int
countDivs x xs k =
	let sums = map (x+) xs in
	length $ filter (\s -> s `rem` k == 0) sums

fdivs :: [Int] -> Int -> Int
fdivs [] _ = 0
fdivs (x:xs) k =
	(countDivs x xs k) + fdivs xs k

main = readInts >>= \[_,k] -> do
	as <- readInts
	putStrLn . show $ fdivs as k
	
