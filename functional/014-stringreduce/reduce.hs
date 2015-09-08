
filterd :: String -> String
filterd [] = []
filterd (x:xs) = x:(filterd $ filter (\c -> c /= x) xs)

main = do
	str <- getLine
	putStrLn $ filterd str
