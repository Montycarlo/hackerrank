import Text.Printf

main = (readLn :: IO Int) >>= \n -> do
	ls <- mapM (\_ -> getLine) [1..n]
	putStrLn  $ (printf "%.1f" (computeDists . wrap $ map (parseInts.words) ls :: Float) :: String)

parseInts = map $ \x -> read x :: Int

wrap xs = xs++[head xs]

computeDists :: Floating a => [[Int]] -> a
computeDists (a:b:cs) = (hyp a b) + computeDists (b:cs)
computeDists _ = 0

hyp :: Floating a => [Int] -> [Int] -> a
hyp [x1,y1] [x2,y2] = sqrt . fromIntegral $ ((x2-x1)^2)+((y2-y1)^2)
