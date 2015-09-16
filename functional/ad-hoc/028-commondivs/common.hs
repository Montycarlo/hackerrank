import qualified Data.Set as S

primes' _ [] = []
primes' prev (x:xs) 
	| divisor = primes' prev xs
	| otherwise = x:primes' (prev++[x]) xs
	where divisor = any (\y -> (rem x y) == 0) prev

primes = 1:2:primes' [2] [3..]

findDivs' n (p:ps)
	| rem n p == 0 = p:rest
	| p^2 > n = []
	| otherwise = rest 
	where rest = findDivs' n ps

findPrimeDivs n = findDivs' n primes
findDivs n = n:findDivs' n [1..]

findDivisorsOf' n acc [] = []
findDivisorsOf' n acc (x:xs)
	| x == 1 = 1:findDivisorsOf' n acc xs
	| acc > n = []
	| rem n acc == 0 = (acc:findDivisorsOf' n (acc*x) (x:xs)) ++ findDivisorsOf' n acc xs
	| otherwise = []

findDivisorsOf n xs = findDivisorsOf' n 1 xs

doCase _ = do
	s <- getLine
	let [a, b] = map (\x-> read x::Int) $ words s;
			divs_a = S.fromList $ findPrimeDivs a;
			divs_b = S.fromList $ findPrimeDivs b;
			common = S.intersection divs_a divs_b;
			divs = findDivisorsOf (min a b) $ S.toList common;
			divs_s = S.fromList $ filter (\x -> rem (max a b) x == 0) divs
	putStrLn $ show $ S.size divs_s

main = do
	t <- readLn :: IO Int 
	mapM_ doCase [1..t]
