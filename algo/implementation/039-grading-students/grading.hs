
grade :: Int -> IO ()
grade 0 = return ()
grade n = do
  l <- readLn :: IO Int
  putStrLn . show $ gradeCase l
  grade (n-1)

gradeCase :: Int -> Int
gradeCase l
  | l < 38 = l
  | l `rem` 5 >= 3 = l + (5-l `rem` 5)
  | otherwise = l

main = do
  n <- readLn :: IO Int
  grade n
