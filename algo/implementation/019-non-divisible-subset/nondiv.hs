readInts :: IO [Int]
readInts = getLine >>= \l -> return $ map read $ words l

main = do
	[_, k] <- readInts
	ns <- readInts
	putStrLn . show $ reduceDivs ns k

reduceDivs ns k = reduceDivs' k $ map (\v -> v `rem` k) ns

reduceDivs' :: Int -> [Int] -> Int
reduceDivs' k ns = 
	let 
		midpoint = floor $ (fromIntegral (k-1))/2.0
		ns_counts = map (getDivPair k ns) [1..midpoint]
	in
	(isEven k) + (resultsZero ns) + sum ns_counts 

getDivPair :: Int -> [Int] -> Int -> Int
getDivPair k ns n = max as bs
	where
		as = length $ filter (n==) ns
		bs = length $ filter (k-n==) ns

firstMax1 [] = []
firstMax1 ns@(0:xs) = ns
firstMax1 (x:xs) = (1:xs)

resultsZero ns
	| any (0==) ns = 1
	| otherwise = 0

isEven k
	| k `rem` 2 == 0 = 1
	| otherwise = 0
