import Data.Char
main = getLine >> getLine >>= \l -> getLine >>= \k_s -> do
  let k = read k_s :: Int
  putStrLn $ encrypt l k

encrypt [] _ = []
encrypt (l:ls) k
  | isLetter l = (encrypt' l k):encrypt ls k
  | otherwise = l:encrypt ls k
encrypt' l k
  | isUpper l = chr $ (+) (ord 'A') $ (ord l - ord 'A' + k) `rem` 26
  | otherwise = chr $ (+) (ord 'a') $ (ord l - ord 'a' + k) `rem` 26
  
