
main = getLine >> getLine >>= \l -> do
  let unique = scanUnique' l []
      uniquePairs = getPairs unique
      uniqueStrs = map (filterOut l) uniquePairs
      validStrs = filter valid uniqueStrs
  putStrLn . show $ maximum $ 0:(map length validStrs)

scanUnique' [] as = as
scanUnique' (l:ls) as
  | any (l==) as = scanUnique' ls as
  | otherwise = scanUnique' ls (l:as)

getPairs [] = []
getPairs (x:xs) = (map (\y -> (x,y)) xs) ++ getPairs xs
  
filterOut l (a,b) = filter (\c -> c == a || c == b) l

valid [] = False
valid (x:xs) = valid' xs x
valid' [] _ = True
valid' (y:ys) x
  | x == y = False
  | otherwise = valid' ys y
