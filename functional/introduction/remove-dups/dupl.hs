process [] = []
process (x:xs) = x:(process $ filter (\y -> y /= x) xs)

main = do
	input <- getLine
	putStrLn $ process input
