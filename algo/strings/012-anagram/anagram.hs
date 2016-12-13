import Data.List
main = readLn >>= \n -> mapM_ doCase [1..n]

doCase _ = getLine >>= putStrLn . show . anagram

anagram l
  | length l `rem` 2 == 1 = -1
  | otherwise = countChanges (take len2 l) (take len2 $ reverse l)
  where
    len_ = length l
    len2 = (floor $ (fromIntegral len_)/2.0)

countChanges [] xs = length xs
countChanges (x:xs) ys = countChanges xs (delete x ys)
