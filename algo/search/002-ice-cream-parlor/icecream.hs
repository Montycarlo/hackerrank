import Data.List
main = readLn >>= \n -> mapM_ doTrip [1..n]

readInts :: IO [Int]
readInts = getLine >>= return . (map read) . words

doTrip _ = do
  m <- readLn
  getLine
  ns <- readInts
  putStrLn . unwords . (map show) . sort $ equalCosts m ns 1

equalCosts :: Int -> [Int] -> Int -> [Int]
equalCosts m (n:ns) ni =
  case nodes of
    Nothing -> equalCosts m ns (ni+1)
    Just x -> [ni,x]
  where nodes = findCountedIndex m n ns (ni+1)

findCountedIndex m n [] _ = Nothing
findCountedIndex m n (n2:ns) i
  | n+n2 == m = Just i
  | otherwise = findCountedIndex m n ns (i+1)
