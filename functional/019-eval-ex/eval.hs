
fact 0 = 1
fact n = n * fact (n-1)

solve_ex :: Double -> Int -> Int -> Double
solve_ex x n maxN
    | n >= maxN = 0
    | otherwise = x ^ n/(fromIntegral $ fact n) + solve_ex x (n+1) maxN

solve :: Double -> Double
solve x = 1 + solve_ex x 1 10

main :: IO ()
main = getContents >>= mapM_ print. map solve. map (read::String->Double). tail. words
