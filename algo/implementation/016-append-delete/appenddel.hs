
main = do
  s1 <- getLine
  s2 <- getLine
  k_s <- getLine
  let k = read k_s :: Int
  putStrLn $ canTransform s1 s2 k

canTransform :: String -> String -> Int -> String
canTransform s1 s2 k = 
  let (del,add) = prefixed s1 s2 in
  canTransform' del add k (length s1) (length s2)

canTransform' del add k mlen nlen
  | del + add > k = "No"
  | del + add == k = "Yes"
  | (del + add - k) `rem` 2 == 0 = "Yes"
  | del == mlen = "Yes"
  | mlen + nlen <= k = "Yes"
  | otherwise = "No"

prefixed [] s2s = (0, length s2s)
prefixed s1s [] = (length s1s, 0)
prefixed (s1:s1s) (s2:s2s)
  | s1 == s2 = prefixed s1s s2s
  | otherwise = (1 + length s1s, 1+length s2s)


