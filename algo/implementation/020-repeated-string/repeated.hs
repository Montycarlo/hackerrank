
main = do
	str <- getLine
	n <- readLn :: IO Int
	putStrLn . show $ countAs str n

countAs :: String -> Int -> Int
countAs str n =
	let 
		strlen = length str
		c_str_a = length $ filter ('a'==) str 
	in
	c_str_a * (floor $ fromIntegral n / fromIntegral strlen) + (countAs' str (n `rem` strlen))

countAs' _ 0 = 0
countAs' (x:xs) n
	| x == 'a' = 1 + rest
	| otherwise = rest
	where rest = countAs' xs (n-1)

