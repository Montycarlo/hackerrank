
readInts :: IO [Int]
readInts = getLine >>= \l -> return $ map read $ words l

main = readInts >>= \[t] -> mapM_ doCase [1..t]

doCase _ = readInts >>= putStrLn . show . findSquares

squares n = (n*n):squares (n+1)

findSquares [a,b] = findSquares' b $ filter (a<=) (squares 1)
findSquares' max (x:xs)
	| x > max = 0
	| otherwise = 1 + findSquares' max xs

