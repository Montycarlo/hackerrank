import Data.List

type Hunger = (Integer,Integer)

calcHunger' p (app,hap) = app + (toInteger p)*hap

mapCostsForP :: Int -> [Hunger] -> [Integer]
mapCostsForP p hs = map (calcHunger' p) hs

findMaxPforHunger :: Integer -> Integer -> Integer -> Double
findMaxPforHunger mangoes appt happy = (fromIntegral mangoes - fromIntegral appt) / (fromIntegral happy)

maxPhunger :: Integer -> Hunger -> (Double,Hunger)
maxPhunger mans hung@(ap,ha) = (findMaxPforHunger mans ap ha, hung)

sortHunger :: (Double,Hunger) -> (Double,Hunger) -> Ordering
sortHunger (x,_) (y,_) = compare x y

sumOfHunger :: Int -> [Hunger] -> Integer
sumOfHunger p hungers = sum $ mapCostsForP p hungers

countUntilOver _ n [] _ = n
countUntilOver mang n ((max,hung):xs) acc
	| s > mang = n
	| s == mang = n+1
	| otherwise = countUntilOver mang (n+1) xs (hung:acc)
	where s = sumOfHunger n (hung:acc)

countUntilOverBin :: Integer -> (Int,Int) -> [Hunger] -> Int
countUntilOverBin mang (n_l,n_h) xs
	| n_l == n_h = n_l
	| s == mang = n_mid
	| s > mang = countUntilOverBin mang (n_l,n_mid-1) xs
	| otherwise = countUntilOverBin mang (n_mid,n_h) xs
	where n_mid = ceiling $ fromIntegral (n_l+n_h)/2.0;
				s = sumOfHunger (n_mid-1) $ take n_mid xs

main = do
	nm <- getLine
	app_str <- getLine
	hap_str <- getLine
	let mapInt = \xs -> map (\y -> read y::Integer) $ words xs;
			[n_s,m_s] = words nm;
			n = read n_s::Int;
			m = read m_s::Integer;
			apps = mapInt app_str;
			haps = mapInt hap_str;
			hungers = reverse . sortBy sortHunger $ map (maxPhunger m) $ zip apps haps;
			hungers_ = map (\(_,h)->h) hungers

	putStrLn . show $ countUntilOverBin m (0,n) hungers_
