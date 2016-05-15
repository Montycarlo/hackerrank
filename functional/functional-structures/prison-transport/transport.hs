
type Connection = (Int,Int)
type PrisonGroup = [Int]

connectChains :: Connection -> [PrisonGroup] -> [PrisonGroup]
connectChains (p,q) ps =
	

parseGraph 0 chains = return chains
parseGraph t chains = do
	pair_str <- getLine
	let [p,q] = map read $ words pair_str;
			conn = (p,q)
	parseGraph (t-1) chains

main = do
	n <- readLn :: IO Int
	m <- readLn :: IO Int
	let singletons = [ [x] | x <- [1..n] ];
	graph <- parseGraph m singletons
	putStrLn $ show singletons
