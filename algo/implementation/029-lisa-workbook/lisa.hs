readInts :: IO [Int]
readInts = getLine >>= return . (map read) . words

main = do
	[_,k] <- readInts
	ts <- readInts
	print $ countSpecialPages k ts 1

countSpecialPages k [] p = 0
countSpecialPages k (t:ts) p = 
	let (sp, p') = countSpecialPages' k (1,t) p 0 in
	sp + countSpecialPages k ts (p'+1)

countSpecialPages' k (t_c,t) p cs
	| t-t_c+1 <= k =
		(qidSpecial t_c t cs p,p)
	| otherwise =
		countSpecialPages' k (t_c+k,t) (p+1) (qidSpecial t_c (min (t_c+k-1) t) cs p)

qidSpecial l r cs p
	| p <= r && p >= l = (cs+1)
	| otherwise = cs
