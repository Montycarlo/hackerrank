main = do
	p <- readLn :: IO Int
	q <- readLn :: IO Int
	putStrLn . printKaprekar $ filter isKaprekar [p..q]

printKaprekar [] = "INVALID RANGE"
printKaprekar ks = unwords $ map show ks

isKaprekar x
	| l+r == x = True
	| otherwise = False
	where 
		sqr = x*x
		d = countDigits x
		l = popDigits sqr d
		r = sqr - (floor $ (fromIntegral l) * 10 ** (fromIntegral d))

popDigits :: Int -> Int -> Int
popDigits n 0 = n
popDigits n d = popDigits (floor $ fromIntegral n/10.0) (d-1)

countDigits :: Int -> Int
countDigits n
	| n < 10 = 1
	| otherwise = 1 + countDigits (floor $ fromIntegral n/10.0)
