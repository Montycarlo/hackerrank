
fib tn_1 tn_2 n i
	| i < n = fib tn tn_1 n (i+1)
	| otherwise = tn
	where tn = tn_1^2 + tn_2

main = do
	l <- getLine
	let [a,b,n] = map (\x->read x::Integer) $ words l
		
	putStrLn . show $ fib b a n 3
