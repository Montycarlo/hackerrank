import Data.Char
main = do
  a <- getLine
  b <- getLine
  putStrLn . show $ findDiffChars a b

findDiffChars a b = sum $ map absdiff $ zip (freqs a) (freqs b)

absdiff (a,b) = abs $ a-b

freqs xs = freqs' xs $ replicate 26 0
freqs' [] fs = fs
freqs' (x:xs) fs = freqs' xs fs'
  where fs' = inc (ord x - ord 'a') fs

inc 0 (x:xs) = (x+1):xs
inc n (x:xs) = x:inc (n-1) xs
