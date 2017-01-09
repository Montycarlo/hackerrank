import Data.List
main = getLine >> getLine >>= print . findMinDist . words

findMinDist :: [String] -> Int
findMinDist xs = findLowest (-1) $ mapIndexes xs 1
	
mapIndexes [] _ = []
mapIndexes (x:xs) n = (n,x):mapIndexes xs (n+1)

findLowest n [] = n
findLowest n ((x_i,x):xs) = 
	let 
		(xsame, rest) = partition (\(_,y) -> x == y) xs
		lowestX_d = calcLowestN (-1) x_i xsame
	in
	if (lowestX_d > -1 && lowestX_d < n) || n == -1
		then findLowest lowestX_d rest
		else findLowest n rest

calcLowestN n _ [] = n
calcLowestN n li ((l2i,_):ls)
	| n == -1 = calcLowestN n2 l2i ls
	| n2 < n = calcLowestN n2 l2i ls
	| otherwise = calcLowestN n l2i ls
	where n2 = l2i - li
