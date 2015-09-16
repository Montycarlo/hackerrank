-- find a infinite list of primes

doCase _ = do
	s <- getLine
	let [a, b] = map (\x-> read x::Int) $ words s
	putStrLn $ show a

main = do
	t <- readLn :: IO Int 
	mapM_ doCase [1..t]
