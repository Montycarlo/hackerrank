findPrefix :: String -> String -> String -> Int -> (String, String, String, Int)
findPrefix xs [] p pl = (xs, [], p, pl)
findPrefix [] ys p pl = ([], ys, p, pl)
findPrefix a@(x:xs) b@(y:ys) p pl
	| x == y = findPrefix xs ys (x:p) (pl+1)
	| otherwise = (a, b, p, pl)

showX x len = do
	putStr $ show $ len
	putStr " "
	putStrLn x

main = do
	x <- getLine
	y <- getLine
	let (x', y', p, p_len) = findPrefix x y [] 0
	showX (reverse p) p_len
	showX x' $ length x'
	showX y' $ length y'
	
