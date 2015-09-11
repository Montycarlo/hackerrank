mergeSet :: [Int] -> [Int] -> [Int]
mergeSet xs [] = xs
mergeSet xs (y:ys)
	| any (y==) xs = rest
	| otherwise = y:rest
	where rest = mergeSet xs ys

commonSets :: Eq a => [a] -> [a] -> Bool
commonSets _ [] = False
commonSets xs (y:ys)
	| any (==y) xs = True
	| otherwise = commonSets xs ys

mergeSubsets [] a b = [mergeSet a b]
mergeSubsets (n:ns) a b
	| commonSets n a = mergeSubsets ns b $ mergeSet n a
	| commonSets n b = mergeSubsets ns a $ mergeSet n b
	| otherwise = n:mergeSubsets ns a b

readNations :: [[Int]] -> Int -> IO [[Int]]
readNations nations 0 = return nations
readNations nations n = do
	l <- getLine 
	let [a,b] = map (\x->read x::Int) $ words l;
			newNations = mergeSubsets nations [a] [b]
	readNations newNations (n-1)

countTeamPairs :: [Int] -> [Int] -> Int
countTeamPairs x y = length x * length y

countPermutations :: [[Int]] -> Int
countPermutations [x] = 0
countPermutations (x:ys) =
	let p_x = foldl (\a y -> a+ countTeamPairs x y) 0 ys in
	p_x + countPermutations ys

main = do
	fl <- getLine
	let [n,i] = map (\x->read x::Int) $ words fl;
			astronauts = [[i] | i <- [0..(n-1)]]

	nations <- readNations astronauts i
	--mapM_ (putStrLn . show) astronauts
	--putStrLn "~~~~~~~"
	--mapM_ (putStrLn . show) nations
	putStrLn . show $ countPermutations nations
