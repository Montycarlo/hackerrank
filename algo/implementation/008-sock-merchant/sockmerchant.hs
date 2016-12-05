
readInts = getLine >>= \x -> return $  map (\y -> read y :: Int) $ words x

pull x [] acc = (False, acc)
pull x (y:ys) acc
  | x == y = (True, acc ++ ys)
  | otherwise = pull x ys (acc ++[y])
  
red [] _ = 0
red (x:xs) ys
  | within = 1 + red xs r
  | otherwise = red xs (x:ys)
  where (within, r) = pull x ys []

main = getLine >> do
  ns <- readInts
  putStrLn . show $ red ns []