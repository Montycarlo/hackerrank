
isPrime' :: Int -> Int -> Bool
isPrime' n i
	| i*i > n = True
	| (n `mod` i == 0) || (n `mod` (i+2) == 0) = False
	| otherwise = isPrime' n (i+6)

isPrime n
	| n <= 1 = False
	| n <= 3 = True
	| (n `mod` 2 == 0) || (n `mod` 3 == 0) = False
	| otherwise = isPrime' n 5

pmap True = "Prime"
pmap False = "Not prime"

getIsPrime = do
	n <- readLn::IO Int
	putStrLn $ pmap $ isPrime n

main = do
	n <- readLn::IO Int
	mapM_ (\x->getIsPrime) [1..n]
