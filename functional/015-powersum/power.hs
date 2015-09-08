n `nthRoot` x = fst $ until (uncurry(==)) (\(_,x0) -> (x0,((n-1)*x0+x/x0**(n-1))/n)) (x,x/n)

findWays :: [Int] -> [Int] -> Int -> [[Int]]
findWays cs haystack max
	| (sum cs) > max = []
	| (sum cs) == max = [cs]
	| (length haystack) == 0 = []
	| otherwise = (findWays (h:cs) hs max) ++ (findWays cs hs max)
		where (h:hs) = haystack

cntCombs :: [Int] -> Int -> Int
cntCombs ps max = length $ findWays [] ps max

main = do
	x_str <- getLine
	n_str <- getLine
	let x = read x_str :: Int 
	let n = read n_str :: Int 
	let maxN = if n > 1 then (floor $ sqrt $ fromIntegral x) else x
	let pows = map (\x -> x^n) [1..maxN]
	putStrLn $ show $ cntCombs pows x
	
