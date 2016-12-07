
main = getLine >>= countBad >>= putStrLn . show

countBad l = countBad' l (cycle "SOS")
countBad' [] _ = return 0
countBad' (x:xs) (y:ys)
  | x == y = rest
  | otherwise = rest >>= \v -> return (v+1)
    where rest = countBad' xs ys

