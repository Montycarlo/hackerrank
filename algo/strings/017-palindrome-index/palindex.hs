main = readLn >>= \n -> mapM_ doCase [1..n]

doCase _ = getLine >>= putStrLn . show . cutIndex

cutIndex l = cutIndex' l (reverse l) 0 (length l)
cutIndex' [] [] _ _ = -1 
cutIndex' (x:xs) (y:ys) n len
	| x == y = cutIndex' xs ys (n+1) len
	| isPalindrome xs xs_r = n
	| isPalindrome ys ys_r = len - n - 1
	| otherwise = -1
	where 
		xs_r = drop n $ reverse xs
		ys_r = drop n $ reverse ys

isPalindrome xs [] = True	
isPalindrome [] ys = True	
isPalindrome (x:xs) (y:ys)	
	| x == y = isPalindrome xs ys
	| otherwise = False
