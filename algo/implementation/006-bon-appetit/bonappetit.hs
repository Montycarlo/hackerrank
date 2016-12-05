
readInts :: IO [Int]
readInts = do
	l <- getLine
	return $ map read $ words l

ss :: [Int] -> Int -> Int
ss [] _ = 0
ss (x:xs) 0 = ss xs (-1)
ss (x:xs) y = x + ss xs (y-1)

chrStr c1 c2
	| c1 == c2 = "Bon Appetit"
	| otherwise = show $ c1 - c2

main = do
	[n,k] <- readInts
	is <- readInts
	[charge] <- readInts
	let shouldCharge = floor $ fromIntegral (ss is k) / 2.0
	putStrLn $ chrStr charge shouldCharge

