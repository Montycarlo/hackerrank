
main = do
	t <- readLn :: IO Int
	mapM_ doCase [1..t]

readInts :: IO [Int]
readInts = getLine >>= (return . (map read) . words)

doCase _ = do
	bws@[b,w] <- readInts
	xyz@[x,y,z] <- readInts
	putStrLn . show $ calcCost bws xyz

calcCost :: [Int] -> [Int] -> Int
calcCost [b,w] [x,y,z]
	| x+z < y = b*x + w*(x+z)
	| x > y+z = b*(y+z) + w*y
	| otherwise = b*x + w*y
