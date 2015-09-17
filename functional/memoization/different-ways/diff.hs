
count _ 0 = 1
count n k 
	| n == k = 1
	| otherwise = count (n-1) (k-1) + count (n-1) k

doCases 0 = return ()
doCases t = do
	l <- getLine
	let [n,k] = map (\x->read x::Int) $ words l
	putStrLn . show $ count n k
	doCases (t-1)

main = do
	t <- readLn :: IO Int
	doCases t
