
main = do
  n <- readLn :: IO Int
  lines <- mapM (\_ -> getLine) [1..n]
  let 
    lines' = map (map (\d -> read [d] :: Int)) lines
    mouth = cavityMap lines'
  putStrLn mouth

cavityMap :: [[Int]] -> String
cavityMap ls = 
  let 
    showLine = foldl (\a b -> a ++ show b) []
    firstLine = showLine (head ls)
    lastLine = if length ls > 1 then showLine (last ls) else []
    midlines = cavities ls
  in
  unlines $ [firstLine] ++ midlines ++ [lastLine]

cavities :: [[Int]] -> [String]
cavities (a:b:c:ds) = (show (head b) ++ cavityMapRow a b c) : cavities (b:c:ds)
cavities _ = []

cavityMapRow :: [Int] -> [Int] -> [Int] -> String
cavityMapRow (a1:a2:a3:as) (b1:b2:b3:bs) (c1:c2:c3:cs)
  | b2 > a2 && b2 > b1 && b2 > b3 && b2 > c2 = "X" ++ rest
  | otherwise = show b2 ++ rest
  where rest = cavityMapRow (a2:a3:as) (b2:b3:bs) (c2:c3:cs)
cavityMapRow [a1,a2] [b1,b2] [c1,c2] = show b2
cavityMapRow _ _ _ = []

