printMulti :: Int -> Int -> String
printMulti n m = 	(show n) ++ " x " ++ (show m) ++ " = " ++ (show $ m*n)

main = do
	n <- readLn :: IO Int
	mapM_ putStrLn $ map (printMulti n) [1..10]
