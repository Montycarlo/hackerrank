r :: [(Char,Char)] -> IO()
r [] = return ()
r ((p,q):xs) = do
	putChar p
	putChar q
	r xs

main = do
	p <- getLine
	q <- getLine
	r (zip p q)

	
