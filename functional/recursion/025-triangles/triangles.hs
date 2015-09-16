
num1s :: Int -> Int 
num1s n = 1 + 2*n

-- width, 1s, numPadding
padding :: Int -> Int -> Int
padding w ones = floor $ (fromIntegral (w - ones)) / 2

repChar :: Char -> Int -> String
repChar c n 
	| n <= 0 = ""
	| otherwise = c:repChar c (n-1)

triLine :: Int -> Int -> Int -> String
triLine w h line =
	pad ++ ones ++ pad
		where 
			oneCount = num1s line
			padCount = padding w oneCount
			pad = (repChar '_' (padCount+1))
			ones = (repChar '1' oneCount)
	
prtTriLines :: Int -> Int -> Int -> [String]
prtTriLines l w h 
	| l == h = [line]
	| otherwise = line:(prtTriLines (l+1) w h)
		where line = triLine w h l

prtTri w h = prtTriLines 0 w h

padTri :: [String] -> Int -> [String]
padTri [] _ = []
padTri (x:xs) n = (pad ++ x ++ pad):padTri xs n
		where pad = (repChar '_' n)

concatTris :: [String] -> [String] -> [String]
concatTris _ [] = []
concatTris [] _ = []
concatTris (x:xs) (y:ys) = (x++"_"++y):concatTris xs ys

--substriangles, width, height, String
sierpinksi :: Int -> Int -> Int -> [String]
sierpinksi 0 w h = prtTri w h
sierpinksi n w h = 
	let subheight = floor $ (fromIntegral h)/2 in
	let subwidth = floor $ (fromIntegral w)/2-1 in
	let top_tri_pad = floor $ (fromIntegral $ w - subwidth)/2 in
	let 
		subTri = sierpinksi (n-1) subwidth subheight
		topHalf = padTri subTri top_tri_pad
		botHalf = concatTris subTri subTri in

	topHalf ++ botHalf


main = do
	input <- getLine
	let n = read input :: Int
	let w = if n == 0 then 61 else 63
	let tri = sierpinksi n w 31
	putStr $ foldl (\x y -> x ++ y ++ "\n") [] tri
