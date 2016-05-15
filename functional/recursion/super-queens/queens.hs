import Debug.Trace;

constructBoard n =
	replicate n $ replicate n "0"

showBoard Nothing = putStrLn "Impossible"
showBoard (Just b) = mapM_ (putStrLn.unwords) b


placeQueenInRow' :: [String] -> Int -> Maybe ([String],Int)
placeQueenInRow' [] _ = Nothing
placeQueenInRow' (x:xs) p_x
	| x == "0" = Just ("q":xs, p_x)
	| otherwise = 
			let result = placeQueenInRow' xs (p_x+1) in
			case result of
				Nothing -> Nothing
				Just (res, p_x_) -> Just (x:res, p_x_)
placeQueenInRow xs  = placeQueenInRow' xs 0

placeQueen :: [[String]] -> Int -> Maybe ([[String]], Int, Int)
placeQueen []	_		= Nothing
placeQueen (r:rs) p_y =
		let inRow = placeQueenInRow r in
		case inRow of
			Nothing -> 
				let rest = placeQueen rs (p_y+1) in
				case rest of
					Nothing -> Nothing
					Just (rs_,_x,_y) -> Just (r:rs_, _x, _y)
			Just (r_, p_x) -> 
				Just (r_:rs, p_x, p_y)

placeQueens :: [[String]] -> Int -> Maybe [[String]]
placeQueens b 0 = Just b
placeQueens b qc =
	let result = placeQueen b 0 in
	case result of
		Nothing -> Nothing
		Just (r,_x,_y) -> 
			let newBoard = updateThreatened r (_x,_y);
					rest = placeQueens newBoard (qc-1) in
			case rest of
				Nothing -> 
					let b_ = setNodeUnavailble b (_x,_y) in
					placeQueens b_ qc
				Just x -> Just x
						

setNodeUnavailble :: [[String]] -> (Int,Int) -> [[String]]
setNodeUnavailble [] _ = []
setNodeUnavailble (r:rs) (x,y)
	| y == 0 = setNodeUnavailbleRow r x:rs
	| otherwise = r:setNodeUnavailble rs (x,y-1)

setNodeUnavailbleRow :: [String] -> Int -> [String]
setNodeUnavailbleRow [] _ = []
setNodeUnavailbleRow (x:xs) 0 = "-":xs
setNodeUnavailbleRow (x:xs) col = x:setNodeUnavailbleRow xs (col-1)

threatenNode :: String -> String
threatenNode s
	| s == "0" = "-"
	| otherwise = s

updateThreatenedRow' _ _ _ [] = []
updateThreatenedRow' p@(x,y) rc cc (c:cs)
	| x == cc = (threatenNode c):rest
	| y == rc = (threatenNode c):rest
	| abs (y - x) == abs (rc - cc) = (threatenNode c):rest
	| abs (y + x - rc - cc) == 3 = (threatenNode c):rest
	| otherwise = c:rest
	where rest = updateThreatenedRow' p rc (cc+1) cs

updateThreatenedRow :: (Int,Int)	-- Point of last queen
										-> Int				-- Row current
										-> [String]		-- Row of 0's?
										-> [String]
updateThreatenedRow p rc row = updateThreatenedRow' p rc 0 row


updateThreatened' :: [[String]] -> (Int,Int) -> Int -> [[String]]
updateThreatened' [] _ _ = []
updateThreatened' (r:rs) p rc =
	c:rest
	where c = updateThreatenedRow p rc r;
				rest = updateThreatened' rs p (rc+1)
updateThreatened :: [[String]] -> (Int,Int) -> [[String]]
updateThreatened b p = updateThreatened' b p 0

main = do
	n <- readLn :: IO Int
	let board = constructBoard n
	showBoard $ placeQueens board n
