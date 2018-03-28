
data Record = Record Int Int

scoreBreaks ns = let h = head ns in
  scoreBreaks' (Record h 0) (Record h 0) (tail ns)

scoreBreaks' :: Record -> Record -> [Int] -> [Int]
scoreBreaks' (Record _ hCount) (Record _ lCount) [] = [hCount, lCount]
scoreBreaks' rh@(Record h hCount) rl@(Record l lCount) (x:xs)
  | x < l = scoreBreaks' rh (Record x (lCount+1)) xs
  | x > h = scoreBreaks' (Record x (hCount+1)) rl xs
  | otherwise = scoreBreaks' rh rl xs

main = do
  n <- readLn :: IO Int
  ns <- getLine
  putStrLn . unwords . (map show) . scoreBreaks . map (\x -> read x :: Int) $ words ns
