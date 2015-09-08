
hello_worlds n
	| n > 0 = do
			putStrLn "Hello World"
			hello_worlds (n-1)
	| otherwise = return () 

-- Do not modify
main = do
	n <- readLn ::IO Int
	hello_worlds n
