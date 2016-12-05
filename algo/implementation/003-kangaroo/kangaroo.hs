
canMatch [x1, v1, x2, v2]
	| x1 == x2 = "YES"
	| v1 > v2 && (x2-x1) `rem` (v1-v2) == 0 = "YES"
	| otherwise = "NO"

main = do
	l <- getLine
	putStrLn . canMatch $ map (\x->read x::Int) $ words l
