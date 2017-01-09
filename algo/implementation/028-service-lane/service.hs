
readInts :: IO [Int]
readInts = getLine >>= return . (map read) . words

main = do
	[n,t] <- readInts
	ws <- readInts
	mapM_ (doCase ws) [1..t]

doCase ws _ = do
	[i,j] <- readInts
	print . minimum . (take (j-i+1)) $ drop i ws

