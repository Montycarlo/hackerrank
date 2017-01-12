
main = do
	a <- getLine
	b <- getLine
	l_c <- getLine
	let k = read l_c :: Int
	putStrLn $ canTransform a b k

canTranform a b k = 
	| diff <= k= "Yes"
	| otherwise = "No"
	where diff = tailDiff a b  

tailDiff [] xs = len xs
tailDiff xs [] = len xs
tailDiff (x:xs) (y:ys)
	| x == y = tailDiff xs ys
	| otherwise = 1 + length xs + length ys
