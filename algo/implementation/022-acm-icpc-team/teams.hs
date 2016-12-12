
readInts :: IO [Int]
readInts = getLine >>= return . (map read) . words

main = do
  [n,_] <- readInts
  peeps <- mapM (\_->getLine) [1..n]
  let
    teamValues = map team $ getAllPairs peeps
    bestTeamValue = maximum teamValues
  putStrLn $ show bestTeamValue
  putStrLn . show . length $ filter (bestTeamValue ==) teamValues

team :: (String,String) -> Int
team ([],[]) = 0
team (a:as,b:bs)
  | a == '1' || b == '1' = 1 + rest
  | otherwise = rest
  where rest = team (as,bs)

getAllPairs [] = []
getAllPairs (x:xs) = (map (\y->(x,y)) xs) ++ getAllPairs xs
