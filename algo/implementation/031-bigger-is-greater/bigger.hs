import Data.List
main = do
  n <- readLn :: IO Int
  mapM_ doCase [1..n]

doCase _ = getLine >>= putStrLn . increment

increment str = 
  case findSwapPair str of
    Nothing -> "no answer"
    Just (left,c1,mid,c2,right) -> left ++ [c2] ++ (sort $ c1:mid++right)

findSwapPair :: String -> Maybe (String,Char,String,Char,String)
findSwapPair [] = Nothing
findSwapPair (x:xs) =
  case findSwapPair xs of
    Nothing -> 
      case findLarger x xs [] Nothing of
        Nothing -> Nothing
        Just (l,c,r) -> Just ([],x,l,c,r)
    Just (before,x2,mid,c,right) -> Just (x:before,x2,mid,c,right)

findLarger :: Char -> String -> String -> Maybe (String,Char,String) -> Maybe (String,Char,String)
findLarger c [] lacc accum = accum
findLarger c (s:ss) lacc accum 
  | c < s = 
    let 
      thispoint = Just (lacc, s, ss)
      accum' = case accum of
       Nothing -> thispoint
       Just a1@(_,c2,_) -> if c2 < s then Just a1 else thispoint
    in
    findLarger c ss (lacc++[s]) accum'
  | otherwise = findLarger c ss (lacc++[s]) accum

