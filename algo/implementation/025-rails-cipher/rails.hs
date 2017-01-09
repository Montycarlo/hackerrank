import Data.List
main = getLine >>= putStrLn . encrypt

encrypt str =
	let 
		lensqrt = sqrt . fromIntegral $ length str
		cols = ceiling lensqrt
		matrix = transpose $ chunksOf cols str
	in
	unwords matrix

chunksOf _ [] = []
chunksOf n str = 
	let (chunk, rest) = chunksOf' n str in
	chunk:chunksOf n rest

chunksOf' 0 str = ([], str)
chunksOf' _ [] = ([], [])
chunksOf' n (s:str) =
	let (suffix, rest) = chunksOf' (n-1) str in 
	(s:suffix, rest)
