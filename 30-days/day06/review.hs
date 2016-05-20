main = do
	n <- readLn :: IO Int
	mapM_ doCase $ [1..n]

doCase _ = do
	s <- getLine
	let (e,o) = extract s
	putStrLn $ e ++ " " ++ o

extract s = extract' s True ([],[])
extract' :: String ->  Bool -> (String,String)-> (String,String)
extract' [] _ r = r
extract' (s:ss) b (e,o)
	| b = rest (e++[s], o)
	| otherwise = rest (e, o++[s])
	where rest = extract' ss (not b)
	
