import Debug.Trace

readInts :: IO [Int]
readInts = getLine >>= \x -> return $ map read $ words x

main = readInts >>= \[i,j,k] -> putStrLn . show $ findDays [i..j] k

findDays [] _ = 0
findDays (d:ds) k
  | beaut k d = 1 + findDays ds k
  | otherwise = findDays ds k

beaut k d = 
  let d2 = read (reverse $ show d) :: Int
      dd = abs $ d2-d 
      re = dd `rem` k in
  trace (show re) (re==0)


