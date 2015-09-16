swap_evens :: String -> String
swap_evens [] = []
swap_evens [x] = [x]
swap_evens (x:xs) = 
	[head xs, x] ++ swap_evens (tail xs)

permute :: Int -> IO()
permute 0 = return  ()
permute n = do
	str <- getLine
	let swapped = swap_evens str
	putStrLn swapped
	permute (n-1)

main = do
	cs <- getLine
	let n = read cs :: Int
	permute n 
