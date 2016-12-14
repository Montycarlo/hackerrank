import Data.List

main = getLine >>= putStrLn . canPalindrome . shiftPairs

canPalindrome (l,xs)
  | length xs == 0 || (length xs == 1 && l `rem` 2 == 1) = "YES"
  | otherwise = "NO"

shiftPairs xs = shiftPairs' xs [] 0
shiftPairs' [] ys n = (n,ys)
shiftPairs' (x:xs) ys n
  | any (x==) ys = shiftPairs' xs (delete x ys) (n+1)
  | otherwise = shiftPairs' xs (x:ys) (n+1)
