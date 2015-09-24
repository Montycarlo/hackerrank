import Data.List
import Debug.Trace

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
			hungers = reverse . sortBy sortHunger $ map (maxPhunger m) $ zip apps haps

	--putStrLn . show $ hungers
	putStrLn . show $ countUntilOver m 0 hungers []
