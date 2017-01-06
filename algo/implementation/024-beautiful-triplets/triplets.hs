readInts :: IO [Int]
readInts = getLine >>= (return . (map read) . words)

main = do
	[n,d] <- readInts
	ns <- readInts
	print $ countBeauts d n ns

countBeauts d n ns
	| n < 3 = 0
	| otherwise = countBeauts' d ns

countBeauts' d [n] = 0
countBeauts' d (n:ns) = (countBeauts'' d n ns) + countBeauts' d ns

countBeauts'' d a_i [] = 0
countBeauts'' d a_i (n:ns)
	| dist == d = countBeauts''' d n ns
	| dist > d = 0
	| otherwise = countBeauts'' d a_i ns
	where dist = n - a_i

countBeauts''' d a_j [] = 0
countBeauts''' d a_j (n:ns)
	| dist == d = 1
	| dist > d = 0
	| otherwise = countBeauts''' d a_j ns
	where dist = n - a_j
