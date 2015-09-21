import Debug.Trace
type PrimePower = (Integer,Integer)

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
  | otherwise      = _p:getPrimePowers' n ps

getPrimePowers :: Integer -> [Integer] -> [(Integer,Integer)]
getPrimePowers n xs = getPrimePowers' n $ map (\x->(x,0)) xs

findPrimePowers n = getPrimePowers n . tail $ findPrimeDivs n

intersectPowerSets' :: [PrimePower] -> PrimePower -> [PrimePower]
intersectPowerSets' [] x = [] 
intersectPowerSets' (y@(y_b,y_p):ys) x@(x_b,x_p)
  | y_b == x_b = [(y_b, if y_p < x_p then y_p else x_p)]
  | otherwise  = intersectPowerSets' ys x

intersectPowerSets :: [PrimePower] -> [PrimePower] -> [PrimePower]
intersectPowerSets ns [] = []
intersectPowerSets ns (x:xs) = 
		(intersectPowerSets' ns x) ++ intersectPowerSets ns xs	

--cmpPrimePowers :: PrimePower -> PrimePower -> Ordering
--cmpPrimePowers (b,_) (c,_)
--	| b < c = LT
--	| b == c = EQ
--	| otherwise = GT

unionPowerWith :: [PrimePower] -> PrimePower -> [PrimePower]
unionPowerWith [] pp = [pp]
unionPowerWith (x@(x_b,x_p):xs) pp@(pp_b,pp_p) 
	| x_b == pp_b = (x_b,x_p+pp_p):xs
	| otherwise		= x:unionPowerWith xs pp

unionPowerSets :: [PrimePower] -> [PrimePower] -> [PrimePower]
unionPowerSets [] x = x
unionPowerSets (x:xs) ys = unionPowerSets xs $ unionPowerWith ys x 

findPowerList :: [Integer] -> [PrimePower]
findPowerList ns = 
	foldl (\x y -> unionPowerSets y x) [] $ map findPrimePowers ns
			

main = do
	n <- getLine
	a_str <- getLine
	m <- getLine
	b_str <- getLine
	let a = findPowerList . map read $ words a_str;
			b = findPowerList . map read $ words b_str;
			com = intersectPowerSets a b;

	putStrLn . show . (\x -> mod x 1000000007) . product $ map (\(x,y)->x^y) com
			
	
