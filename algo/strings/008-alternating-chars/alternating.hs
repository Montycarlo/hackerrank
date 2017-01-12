
main = do
  n <- readLn :: IO Int
  mapM_ doCase [1..n]
  
doCase _ = getLine >>= putStrLn . show . countDel

countDel [] = 0
countDel [_] = 0
countDel (x:y:zs)
  | x == y = 1 + rest
  | otherwise = rest
  where rest = countDel (y:zs)
