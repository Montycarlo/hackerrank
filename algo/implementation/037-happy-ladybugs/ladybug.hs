import Data.List
import Data.Maybe

main = do 
  g <- readLn :: IO Int
  mapM_ doCase [1..g]

doCase _ = getLine >> getLine >>= putStrLn . ladyBugsHappy

ladyBugsHappy xs
  | allPaired && hasSlot = "YES"
  | allPaired && allHappy xs = "YES"
  | otherwise = "NO"
  where
    allPaired = countsAboveOne xs
    hasSlot = hasEmptySlot xs

hasEmptySlot xs
  | isNothing e = False
  | otherwise = True
  where e = find ('_'==) xs

countsAboveOne xs
  | null counts1 = True
  | otherwise = False
  where
    counts1 = filter (1==) . map (\d -> length $ findIndices (d==) xs) . filter ('_'/=) $ nub xs

allHappy [] = True
allHappy [x] = False
allHappy (x:y:zs)
  | x == y = allHappy (dropWhile (x==) zs)
  | otherwise = False
