import Data.Char
main = do
	t <- readLn :: IO Int
	mapM_ doCase [1..t]

doCase _ = getLine >>= putStrLn . isFunny

isFunny str = isFunny' str (reverse str)
isFunny' [_] [_] = "Funny"
isFunny' (a:b:cs) (x:y:zs)
	| cs_ord == zs_ord = isFunny' (b:cs) (y:zs)
	| otherwise = "Not Funny"
	where
		cs_ord = abs $ ord a - ord b
		zs_ord = abs $ ord x - ord y
