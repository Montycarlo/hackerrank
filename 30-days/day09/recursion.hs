
factorial n
	| n <= 1 = 1
	| otherwise = n * factorial (n-1)

main = do
	n <- readLn :: IO Int
	putStrLn.show $ factorial n
