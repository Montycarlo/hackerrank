

main = (readLn :: IO Int) >>= \x -> mapM_ doCase [1..x]

doCase _ = do
	n <- readLn :: IO Int
	relations <- readRelations n
	putStrLn $ case (checkUnique relations) of
		False -> "NO"
		True -> "YES"

readRelations :: Int -> IO [[String]]
readRelations 0 = return []
readRelations x = getLine >>= \line -> do
	rest <- readRelations (x-1)
	return $ (words line):rest

checkUnique xs = checkUnique' xs []
checkUnique' :: [[String]] -> [[String]] -> Bool
checkUnique' [] _ = True
checkUnique' ([a,b]:xs) before
	| any (\[a2,b2] -> a==a2 && b2/=b) before = False
	| otherwise = checkUnique' xs ([a,b]:before)
