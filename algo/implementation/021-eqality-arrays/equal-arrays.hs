
main = getLine >> getLine >>= \l -> putStrLn . show . equalize $ map read $ words l

equalize ns = 
	let 
		mn = maximum ns
		nlens = map (\n_ -> length $ filter (n_==) ns) [1..mn]
	in
	(length ns) - maximum nlens

