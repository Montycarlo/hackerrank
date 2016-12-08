import qualified Data.Set as Set
main = do
 nl <- getLine
 let n = read nl :: Int
 s1l <- getLine
 let s1 = uniqu s1l []
 ss <- mapM (\_ -> getLine) [2..n]
 putStrLn . show . length $ foldl (\m l -> filter (`elem` l) m) s1 ss
 
uniqu [] xs = xs
uniqu (x:xs) ys
  | any (x==) ys = uniqu xs ys
  | otherwise = uniqu xs (x:ys)

