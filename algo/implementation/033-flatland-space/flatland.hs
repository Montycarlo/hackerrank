import Data.List

readInts :: IO [Int]
readInts = getLine >>= return . (map read) . words

main = do
  [n,m] <- readInts
  ms <- readInts
  let ms' = sort ms
  print $ findMaxDist n ms'

findMaxDist :: Int -> [Int] -> Int
findMaxDist n as = floor . (/2.0) . fromIntegral  $ maximum (2*a:2*(n-b-1):findDiffs a (tail as))
  where 
    a = head as
    b = last as

findDiffs _ [] = []
findDiffs a (x:xs) = x-a:findDiffs x xs

