
pent :: Int -> Int
pent 0 = 0 
pent 1 = 1
pent maxN  = 1+(n * (2*a+ (n-1)*d)) `quot` 2-- (maxN-1)/2
		where 
				a = 4
				d = 3
				n = maxN-1

doCases 0 = return ()
doCases n = do
	n_str <- getLine
	putStrLn . show $ pent (read n_str :: Int)
	doCases (n-1)

main = do
	cases <- getLine
	let caseCount = read cases :: Int
	doCases caseCount
