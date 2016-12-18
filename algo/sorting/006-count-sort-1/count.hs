
main = getLine >> getLine >>= putStrLn . unwords . (map show) . count100 . (map read) . words
count100 xs = map (\n -> length $ filter (n==) xs)  [0..99]
