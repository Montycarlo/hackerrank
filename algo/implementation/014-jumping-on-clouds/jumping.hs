readInts :: IO [Int]
readInts = getLine >>= \l -> return $ map read $ words l

main = getLine >> readInts >>= \cs -> putStrLn . show $ countJumps $ tail cs

countJumps [] = 0
countJumps [x] = 1
countJumps (x1:x2:xs)
  | x2 == 0 = 1+countJumps xs
  | otherwise = 1+countJumps (x2:xs)
