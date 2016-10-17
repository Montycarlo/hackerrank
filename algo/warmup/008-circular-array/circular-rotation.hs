rotate k n ns =
	let (t,h) = splitAt (n-(k `mod` n)) ns in
	h++t
	
readInts :: IO [Int]
readInts = readArray >>= \rs -> (return $ map read rs)
readArray :: IO [String]
readArray = getLine >>= \x -> (return $ words x)

main = readInts >>= \[n,k,q] -> readArray >>= \ns ->
	let ns_rot = rotate k n ns
	in mapM_ (handleQueries ns_rot) [1..q]

handleQueries :: [String] -> Int -> IO ()
handleQueries xs _ = (readLn :: IO Int) >>= \q -> putStrLn $ xs !! q
