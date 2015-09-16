import Debug.Trace

blankLine w = replicate w '_'

blankLines :: Int -> Int -> [String]
blankLines 0 w = []
blankLines n w = blankLine w:blankLines (n-1) w

calcPad :: Int -> Int
calcPad w = floor $ (fromIntegral w)/2

stick :: Int -> String
stick w =
	padL ++ "1" ++ padR 
	where
		padRc = calcPad w
		padLc = padRc 
		padL = replicate padLc '_'
		padR = replicate padRc '_'

stickLine :: Int -> Int -> [String]
stickLine 0 w = []
stickLine n w = stick w:stickLine (n-1) w

transitPadding n = 1 + 2*n
transitLPadding n correct= correct - n

transitLine :: Int -> Int -> Int -> [String]
transitLine h w 0 = []
transitLine h w cL = 
	let n = floor $ (fromIntegral cL) - ((fromIntegral h)/2+1) in
	if n < 0 then
		stick w:rest
	else 
		let correctPad = calcPad w in
		let pad_l = -1 + correctPad - n in
		let pad_mid = transitPadding n in
		
		let padLeft = replicate pad_l '_' in
		let padMid = replicate pad_mid '_' in
		(padLeft++"1"++padMid++"1"++padLeft):rest
	where rest = transitLine h w (cL-1)

appendTree :: [String] -> [String] -> [String]
appendTree [] _ = []
appendTree _ [] = []
appendTree (x:xs) (y:ys) = (x++"_"++y):appendTree xs ys

tree :: Int -> Int -> Int -> Int -> [String]
tree n m w h
	| n == m = 
			blankLines h w
	| otherwise = 
			let topHalf_h = floor $ (fromIntegral h)/2 in
			let botHalf_h = h - topHalf_h in

			let topHalf_w = floor $ (fromIntegral w)/2 in
			let botHalf = transitLine botHalf_h w botHalf_h in

			let topSub = tree (n+1) m topHalf_w topHalf_h in
			let topHalf = appendTree topSub topSub  in
			topHalf ++ botHalf

main = do
	line <- getLine
	let n = read line :: Int
	let l = tree 0 n 63 63 
	let l_padded = map (\x -> replicate 18 '_' ++ x ++ replicate 19 '_') l
	putStr $ foldl (\x y -> x ++ y ++ "\n" ) [] l_padded
