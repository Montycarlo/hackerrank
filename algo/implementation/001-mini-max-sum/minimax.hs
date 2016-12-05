import Data.List

ls = getLine >>= \y -> return $ map (\x -> read x :: Int) $  words y

main = do
  ints <- ls
  let lss = sort ints
  putStr $ show (sum $ take 4 lss) ++ " " ++ show (sum $ tail lss)

