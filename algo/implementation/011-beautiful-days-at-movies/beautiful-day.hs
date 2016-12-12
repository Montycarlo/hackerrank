readInts :: IO [Int]
readInts = getLine >>= \x -> return $ map read $ words x

main = readInts >>= \[i,j,k] -> putStrLn . show $ findDays [i..j] k

findDays [] _ = 0
findDays (d:ds) k
  | beaut k d = 1 + findDays ds k
  | otherwise = findDays ds k

beaut k d = 
  let dd = abs $ (reverseInt d 0)-d 
      re = dd `rem` k in
  re==0

reverseInt :: Int -> Int -> Int
reverseInt n acc
	| n <= 0 = acc
	| otherwise = reverseInt (floor $ fromIntegral n/10.0) (acc*10 + n `rem` 10)
