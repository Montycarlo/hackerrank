
readInts :: IO [Int]
readInts = fmap (map read.words) getLine

main = do
  n <- getLine
  chocs <- readInts
  dm <- readInts
  putStrLn . show $ findSols dm chocs

findSols [d,m] bar = findSols' d rest prefix
  where (prefix,rest) = splitAt m bar

findSols' d [] current = addBar d current
findSols' d (x:xs) current = addBar d current + findSols' d xs nextTail
  where nextTail = (tail current) ++ [x]

addBar d bar
  | sum bar == d = 1
  | otherwise = 0
