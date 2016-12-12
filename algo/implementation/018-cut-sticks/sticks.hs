import Data.List
main = getLine >> getLine >>= \l -> 
  let ns = sort $ map read $ words l :: [Int] in
  cut ns

cut [] = return ()
cut (n:ns) = do
  putStrLn . show $ 1 + length ns 
  cut $ filter (>0) $ map (\n2 -> n2-n) ns
