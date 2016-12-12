
main = do
  t <- readLn :: IO Int
  mapM_ doCase [1..t]

doCase _ = getLine >>= \l -> putStrLn . show . sum $ map (divs (read l)) l

divs :: Int -> Char -> Int
divs i c
  | c_ == 0 = 0
  | i `rem` c_ == 0 = 1
  | otherwise = 0
  where c_ = read [c] :: Int
