
fib' :: Integer -> Integer -> [Integer]
fib' n m = let x = n+m in x:fib' x n
fib = 0:1:(fib' 1 0)

doCases 0 = return ()
doCases t = do
	n <- readLn :: IO Int
	putStrLn . show $ rem (fib !! n) (10^8+7)
	doCases $ t-1

main = do
	t <- readLn :: IO Int
	doCases t
