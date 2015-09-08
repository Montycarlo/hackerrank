str_rotate :: [Char] -> [Char] -> [[Char]]
str_rotate [] ys = [ys]
str_rotate a@(x:xs) ys = (a++ys):str_rotate xs (ys++[x])

doCases :: Int -> IO()
doCases 0 = return ()
doCases n = do
	str <- getLine
	let rotations = str_rotate (tail str) [head str]
	putStrLn (unwords rotations)
	doCases (n-1)

main = do
	lines <- getLine
	let caseCount = read lines :: Int
	doCases caseCount

