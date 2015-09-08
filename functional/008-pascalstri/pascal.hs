pascal_series :: Int -> [Int] -> [Int]
pascal_series x [] = [x]
pascal_series x (y:ys) = (x+y):pascal_series y ys

pascal_line :: Int -> [Int] -> [[Int]]
pascal_line 0 _ = []
pascal_line n last_line =
		(new_line):(pascal_line (n-1) new_line)
			where
				new_line = pascal_series 0 last_line
			

pascal :: Int -> [[Int]]
pascal 0 = []
pascal n = [1]:pascal_line (n-1) [1]

print_triline xs = unwords (map show xs)

main = do
	lines <- getLine
	let linecount = (read lines :: Int)
	let tri = pascal linecount
	putStrLn (unlines (map print_triline tri))
