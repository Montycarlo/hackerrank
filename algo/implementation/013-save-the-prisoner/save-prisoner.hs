
readInts :: IO [Int]
readInts = getLine >>= \l -> return $ map read $ words l
main = do
	[t] <- readInts
	mapM_ doCase [1..t]
wrap0 m 0 = m
wrap0 _ m = m
doCase _ = readInts >>= \[n,m,s] ->
	putStrLn . show $ wrap0 n $ (m+s-1) `rem` n
		
