import Debug.Trace

primes' _ [] = []
primes' prev (x:xs) 
  | divisor = primes' prev xs
  | otherwise = x:primes' (prev++[x]) xs
  where divisor = any (\y -> (rem x y) == 0) prev

primes = 1:2:primes' [2] [3..]

divideUntilCant n 1 = n
divideUntilCant n m
	| rem n m == 0 = divideUntilCant (floor $ fromIntegral n / fromIntegral m) m
	| otherwise = n

findDivs' 1 _ = []
findDivs' n (p:ps)
  | rem n p == 0 = p:findDivs' (divideUntilCant n p) ps
  | p^2 > n = [n]
  | otherwise = findDivs' n ps

findPrimeDivs n = 
	let v = findDivs' n primes in
	if null v then [1] else v

getPrimePowers' 1 ps = ps
getPrimePowers' n [] = error $ "ERROR -> " ++ show n
getPrimePowers' n (_p@(p_b,p_p):ps)
	| rem n p_b == 0 = getPrimePowers' (floor (fromIntegral n/fromIntegral p_b)) ((p_b,p_p+1):ps)
	| otherwise 		 = _p:getPrimePowers' n ps

getPrimePowers :: Integer -> [Integer] -> [(Integer,Integer)]
getPrimePowers n xs = getPrimePowers' n $ map (\x->(x,0)) xs

findPrimePowers n = getPrimePowers n . tail $ findPrimeDivs n

combinePowerSets' :: [(Integer,Integer)] -> (Integer,Integer) -> [(Integer,Integer)]
combinePowerSets' [] x = [x]
combinePowerSets' (y@(y_b,y_p):ys) x@(x_b,x_p)
	| y_b == x_b = (y_b, if y_p > x_p then y_p else x_p):ys
	| otherwise  = y:combinePowerSets' ys x

combinePowerSets :: [(Integer,Integer)] -> [(Integer,Integer)] -> [(Integer,Integer)]
combinePowerSets ns [] = ns
combinePowerSets ns (x:xs) = combinePowerSets (combinePowerSets' ns x) xs
	

smallestC xs = 
	let primePows = foldl (++) [] $ map findPrimePowers xs;
			powerSets = combinePowerSets [] primePows in
	foldl (\x (y_b,y_p) -> x * y_b^y_p) 1 powerSets

main = do
	m <- readLn :: IO Int
	wsl <- getLine
	let ws = map read $ words wsl
	putStrLn . show $ smallestC ws
