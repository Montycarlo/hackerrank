
readInts :: IO [Int]
readInts = getLine >>= \l -> return $ map read $ words l

main = do
  [t] <- readInts
  mapM_ doCase [0..t]

doCase _ = do
  [_,k] <- readInts
  as <- readInts
  let att = length $ filter (0>=) as
  putStrLn $ angry k att

angry k att
  | att >= k = "NO"
  | otherwise = "YES"

