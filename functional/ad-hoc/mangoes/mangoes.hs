import Data.List

type Hunger = (Integer,Integer)

calcHunger' p (app,hap) = app + (toInteger p)*hap

mapCostsForP :: Int -> [Hunger] -> [Integer]
mapCostsForP p hs = map (calcHunger' (p-1)) hs

findSmallestHungerSum :: Int -> [Hunger] -> Integer
findSmallestHungerSum p hs = sum . (take p) $ mapCostsForP p hs;

findLargestSumUnderLimit' :: [Hunger] -> Integer -> (Int,Int) -> Int
findLargestSumUnderLimit' hs max (p,n)
	| p == n		= n
	| s > max		= findLargestSumUnderLimit' hs max (p,p_c-1)
	| s < max		= findLargestSumUnderLimit' hs max (p_c,n)
	| s == max	= p_c
	where p_c = round $ fromIntegral (p+n)/2.0;
				s = findSmallestHungerSum p_c hs
-- | p == 0			= 0
-- | s <= max		= p
-- | otherwise = findLargestSumUnderLimit' hs max ((p-1),n)
--		where s = findSmallestHungerSum p hs
findLargestSumUnderLimit :: [Hunger] -> Integer -> Int -> Int
findLargestSumUnderLimit hs max n = findLargestSumUnderLimit' hs max (1,n)

sortHunger :: Hunger -> Hunger -> Ordering
sortHunger (ap,happ) (ap2,happ2)
	| happ < happ2 = LT
	| happ == happ2 = compare ap ap2
	| otherwise = GT

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
			hungers = sortBy sortHunger $ zip apps haps
	putStrLn . show $ findLargestSumUnderLimit hungers m n
