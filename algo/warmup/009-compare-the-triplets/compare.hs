
readTrip :: IO [Int]
readTrip = do
  l <- getLine
  return $ map (\x -> read x :: Int) $ words l

cmp (as,bs) (a,b) 
  | a > b = (as+1,bs)
  | b > a = (as,bs+1)
  | otherwise = (as,bs)

computeScores as bs = 
  foldl cmp (0,0) (zip as bs)

main = do
  as <- readTrip
  bs <- readTrip
  let (a_s, b_s) = computeScores as bs
  putStrLn $ show a_s ++ " " ++ show b_s
