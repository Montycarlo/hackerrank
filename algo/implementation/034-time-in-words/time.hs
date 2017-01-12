{-# LANGUAGE MultiWayIf #-}

main = do
	h <- readLn :: IO Int
	m <- readLn :: IO Int
	putStrLn $ convert h m

convert :: Int -> Int -> String
convert h m
	| m <= 30 = 
		if	| m == 30 -> "half past " ++ digitToName h
				| m == 15 -> "quarter past " ++ digitToName h
				| m == 0 -> digitToName h ++ " o' clock"
				| otherwise -> 
					if | m == 1 -> digitToName m ++ " minute past " ++ digitToName h
						 | otherwise -> digitToName m ++ " minutes past " ++ digitToName h
	| otherwise = 
		if	| m == 59 -> "one minute to " ++ digitToName (1 + (h `rem` 12))
				| m == 45 -> "quarter to " ++ digitToName (1 + (h `rem` 12))
				| otherwise -> digitToName (60-m) ++ " minutes to " ++ digitToName (1 + (h `rem` 12))

digitToName 1 = "one"
digitToName 2 = "two"
digitToName 3 = "three"
digitToName 4 = "four"
digitToName 5 = "five"
digitToName 6 = "six"
digitToName 7 = "seven"
digitToName 8 = "eight"
digitToName 9 = "nine"
digitToName 10 = "ten"
digitToName 11 = "eleven"
digitToName 12 = "twelve"
digitToName 13 = "thirteen"
digitToName 14 = "fourteen"
digitToName 15 = "fifteen"
digitToName 16 = "sixteen"
digitToName 17 = "seventeen"
digitToName 18 = "eighteen"
digitToName 19 = "nineteen"
digitToName x
	| x < 30 = "twenty " ++ digitToName (x-20)
	| x < 40 = "thirty " ++ digitToName (x-30)
	| otherwise = "BAD"


