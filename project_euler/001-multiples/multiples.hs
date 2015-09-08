compute_sum_factors :: (Num a, RealFrac a) => a -> a -> Integer
compute_sum_factors x y
	| x>y = 0
	| otherwise = floor (n*(n-1)*x/2)
			where n = fromIntegral (floor (y/x) + 1)

compute_sum_3_5 :: (Num a, RealFrac a) => a -> Integer
compute_sum_3_5 x = 
	threes + fives - fifteens
	where 
		threes = compute_sum_factors 3 x
		fives = compute_sum_factors 5 x
		fifteens = compute_sum_factors 15 x

get_reqs 0 = return ()
get_reqs t = do
	x <- readLn 
	let sum = compute_sum_3_5 (x-1) in
		print sum 
	get_reqs (t-1)

main = do
	t <- readLn ::IO Int
	get_reqs t

