
readInts :: IO [Int]
readInts = getLine >>= \l -> return $ map read $ words l

main = do
  [n,k] <- readInts
  cs <- readInts
  putStrLn . show $ calcEnergy n k cs 0 100

calcEnergy n k cs current energy
  | nextPos == 0 = newEnergy
  | otherwise = calcEnergy n k cs nextPos newEnergy
  where nextPos = (current + k) `rem` n
        newEnergy = energy - (lookupCloud cs nextPos)

lookupCloud :: [Int] -> Int -> Int
lookupCloud (c:cs) 0
  | c == 0 = 1
  | otherwise = 3
lookupCloud (c:cs) n = lookupCloud cs (n-1)

