import qualified Data.Set as PQ

-- Credits for Primes fN
--	 Melissa E. O'Neill 
--	 Garrison Jensen
-- Please see this
-- http://www.garrisonjensen.com/2015/05/13/haskell-programs-are-lies.html
primes :: [Integer]
primes = 2:sieve [3,5..]
  where
    sieve (x:xs) = x : sieve' xs (insertprime x xs PQ.empty)

    sieve' (x:xs) table
        | nextComposite == x = sieve' xs (adjust x table)
        | otherwise          = x : sieve' xs (insertprime x xs table)
      where 
        (nextComposite,_) = PQ.findMin table

    adjust x table
        | n == x    = adjust x (PQ.insert (n', ns) newPQ)
        | otherwise = table
      where
        Just ((n, n':ns), newPQ) = PQ.minView table

    insertprime p xs = PQ.insert (p*p, map (*p) xs)

isPrime' n (x:xs)
	| n == x = True
	| n < x = False
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
	| not prime			  = "DEAD"
	| isCentral				= "CENTRAL"
	| leftsPrime			= "LEFT"
	| rightsPrime			= "RIGHT"
	| otherwise				= "DEAD"
	where prime = isPrime $ read id;
				leftsPrime = all (isPrime.read) $ init $ lefts id;
				rightsPrime = all (isPrime.read) $ init $ rights id;
				isCentral = leftsPrime && rightsPrime && prime

doCase n = do
	id <- getLine
	putStrLn $ getArea id

main = do
	t <- readLn :: IO Int
	mapM_ doCase [1..t]
