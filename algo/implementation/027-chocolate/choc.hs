
readInts :: IO [Int]
readInts = getLine >>= return . (map read) . words

main = do
	n <- readLn :: IO Int
	mapM_ doCase [1..n]

doCase _ = readInts >>= \[n,c,m] -> print $ calcChocs n c m

calcChocs n c m = bought + trade bought m
	where bought = floor $ fromIntegral n / fromIntegral c

trade w m
	| w < m = 0
	| otherwise = tradedv + trade wraps m
	where 
		tradedv = floor $ fromIntegral w / fromIntegral m 
		wraps = tradedv + w `rem` m
