import Data.List

data CmpTree a = CmpTree a Int (CmpTree a) (CmpTree a) | Leaf

treeInsert :: CmpTree Int -> Int -> CmpTree Int
treeInsert Leaf v = CmpTree v 1 Leaf Leaf
treeInsert (CmpTree v2 c l r) v
  | v == v2 = CmpTree v2 (c+1) l r
  | v > v2 = CmpTree v2 c l (treeInsert r v)
  | otherwise = CmpTree v2 c (treeInsert l v) r

treeSub :: CmpTree Int -> Int -> CmpTree Int
treeSub Leaf v = Leaf
treeSub n@(CmpTree v2 c l r) v
  | v == v2 = sub n
  | v > v2 = CmpTree v2 c l (treeSub r v)
  | otherwise = CmpTree v2 c (treeSub l v) r

sub (CmpTree v2 c l r)
  | c > 1 = CmpTree v2 (c-1) l r
  | otherwise = promote l r

promote Leaf Leaf = Leaf
promote Leaf m@(CmpTree v c l r) = m
promote (CmpTree v c l r) rr = CmpTree v c l (promote r rr)

treeSearch :: CmpTree Int -> Int -> Int
treeSearch Leaf v = 0
treeSearch (CmpTree v2 c l r) v
  | v == v2 = c
  | v > v2 = treeSearch r v
  | otherwise = treeSearch l v

treeDiff Leaf _ = Leaf
treeDiff t1@(CmpTree v c l r) t2 = CmpTree v (c-treeSearch t2 v) (treeDiff l t2) (treeDiff r t2)

getPositiveCounts :: CmpTree Int -> [Int]
getPositiveCounts Leaf = []
getPositiveCounts (CmpTree v c l r)
  | c > 0 = getPositiveCounts l ++ [v] ++ getPositiveCounts r
  | otherwise = getPositiveCounts l ++ getPositiveCounts r

treeToArray Leaf = []
treeToArray (CmpTree v c l r) = (treeToArray l) ++ [v] ++ (treeToArray r)

getList = getLine >> getLine >>= \ls -> return $ map read $ words ls
main = do
  xs <- getList
  ys <- getList
  let srcTree = foldl treeInsert Leaf ys
  putStrLn . unwords . (map show) . getPositiveCounts $ foldl treeSub srcTree xs
