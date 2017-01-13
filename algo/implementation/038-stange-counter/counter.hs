main = do
  t <- readLn :: IO Int
  print $ counter 3 3 (t-1)
  
counter acc _ 0 = acc
counter x y t
  | t >= x = counter (y*2) (y*2) (t-x)
  | otherwise = x - t
