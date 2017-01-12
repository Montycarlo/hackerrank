{-# LANGUAGE MultiWayIf #-}

readInts :: IO [Int]
readInts = getLine >>= return . (map read) . words

main = getLine >> readInts >>= putStrLn . handout

handout ns = 
	case groupedOdds ns of
		True -> show $ calcBread ns
		False -> "NO"

groupedOdds [] = True
groupedOdds (n:ns)
	| odd n = groupedOdds' n ns
	| otherwise = groupedOdds ns

groupedOdds' n ns =
	let (l,r) = span odd ns in
	if	| even (1+length l) -> groupedOdds r
			| otherwise -> 
				case r of
					[] -> False
					xs -> 
						let (popped,rest) = span even xs in
						case rest of
							[] -> False
							otherwise -> groupedOdds (tail rest)

calcBread [] = 0
calcBread (n:ns)
	| odd n = calcBread' n ns
	| otherwise = calcBread ns

calcBread' n ns =
	let 
		(l,r) = span odd ns 
		segment = (1+length l)
	in
	if	| even segment -> segment + calcBread r
			| otherwise ->
				let (popped,rest) = span even r in
				segment + (length popped)*2 + 1 + calcBread (tail rest)

