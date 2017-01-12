import Data.Char
main = getLine >>= isPangram >>= putStr

isPangram :: String -> IO String
isPangram xs
  | pangram = return "pangram"
  | otherwise = return "not pangram"
  where pangram = (==) 0 $ length $ sieve xs [0..25]

sieve :: String -> [Int] -> [Int]
sieve xs ys = 
  foldl filterer ys xs
  where 
    alphaInd x = (-) (ord $ toLower x) (ord 'a')
    filterer = \ys x -> filter (\y -> alphaInd x /= y) ys
