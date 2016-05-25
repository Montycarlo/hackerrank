
getArrayLine :: a -> IO [Int]
getArrayLine _ = do
	l <- getLine
	return $ map (read::String->Int) $ words l

grabHourGlasses (_:[]) _ _ = []
grabHourGlasses (_x:x:xs) ts bs = ([x] ++ take 3 ts ++ take 3 bs):grabHourGlasses (x:xs) (tail ts) (tail bs)

grab matrix n = grabHourGlasses (matrix !! n) (matrix !! (n-1)) (matrix !! (n+1))

main = do
	inp <- mapM getArrayLine [1..6]
	let hgs = foldl (\x y -> x ++ grab inp y) [] [1..4] 
	let m = maximum $ map sum hgs
	putStrLn $ show m
