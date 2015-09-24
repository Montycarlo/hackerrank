
primes' prev n
	| all (\x->rem n x /= 0) prev = n:primes' (n:prev) (n+2)
	| otherwise = primes' prev (n+1)
primes = 1:2:primes' [2] 3

isPrime' n (x:xs)
	| n == x = True
	| x > n = False
	| otherwise = isPrime' n xs
isPrime n = isPrime' n primes

containsZero :: String -> Bool
containsZero xs = any (\x->'0'==x) xs

rights [] = []
rights xs = (rights $ init xs) ++ [xs]
lefts [] = []
lefts xs = (lefts $ tail xs) ++ [xs]

getArea id
	| containsZero id = "DEAD"
	| isCentral				= "CENTRAL"
	| leftsPrime			= "LEFT"
	| rightsPrime			= "RIGHT"
	| otherwise				= "DEAD"
	where leftsPrime = all (isPrime.read) $ lefts id;
				rightsPrime = all (isPrime.read) $ rights id;
				isCentral = leftsPrime && rightsPrime

doCase n = do
	id <- getLine
	putStrLn $ getArea id

main = do
	t <- readLn :: IO Int
	mapM_ doCase [1..t]
