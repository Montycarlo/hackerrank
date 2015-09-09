printStair 0 m = [replicate m '#']
printStair n m = (spaces ++ hashes) : printStair (n-1) m
	where spaces = replicate n ' ';
				hashes = replicate (m-n) '#'

main = do
	n <- readLn :: IO Int
	mapM_ putStrLn $ printStair (n-1) n 

