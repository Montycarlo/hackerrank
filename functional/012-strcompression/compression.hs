cmprTupl :: (Char, Int) -> String
cmprTupl (x,c)
	| c == 1 = [x]
	| otherwise = x:show c

seqCounts :: String -> Char -> Int -> [(Char, Int)]
seqCounts [] x c = [(x,c)]
seqCounts (y:ys) _ 0 = seqCounts ys y 1
seqCounts (y:ys) x c 
		| y == x = seqCounts ys x (c+1)
		| otherwise = (x,c):seqCounts ys y 1

printEachTupl :: [(Char, Int)] -> IO()
printEachTupl [] = return ()
printEachTupl (x:xs) = do
	putStr (cmprTupl x)
	printEachTupl xs

main = do
	x <- getLine
	let seqs = seqCounts x 'x' 0
	printEachTupl seqs

