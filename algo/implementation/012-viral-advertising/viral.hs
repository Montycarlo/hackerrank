
main = getLine >>= \l -> (return $ read l) >>= \x -> putStrLn . show $ findTotal 1 x 5 0

findTotal :: Int -> Int -> Int -> Int -> Int
findTotal i n willSee acc
	| i == n = acc+liked
	| otherwise = 
		findTotal (i+1) n (3*liked) (acc+liked)
	where liked = calcLiked willSee
		
calcLiked p = floor $ fromIntegral p / 2.0

