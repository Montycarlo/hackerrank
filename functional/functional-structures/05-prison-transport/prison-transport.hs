import Data.Set (Set, singleton, union, member, size, fromList, delete, insert)

type Prison = Set Int
data Tree a = Tree a Bool (Tree a) (Tree a) | Leaf deriving Show

main = (readLn :: IO Int) >>= \n -> (readLn :: IO Int) >>= ferry n

ferry n m = do
	lnks <- mapM (\_->getLine) [1..m]
	let prison = fromList [1..n]
	let grps = rdGrps prison $ map words lnks
	let singles = (n - (sum $ map size grps))
	putStrLn . show $ (+) singles $ sum $ map cost grps

cost :: Set Int -> Int
cost g = ceiling . sqrt . fromIntegral $ size g

rdGrps :: Prison -> [[String]] -> [Set Int]
rdGrps p xs = rdGrps' p xs []
rdGrps' :: Prison -> [[String]] -> [Set Int] -> [Set Int]
rdGrps' _ [] ys = ys
rdGrps' p (x:xs) ys = 
	let (p2,ys2) = mrg p (map (\r->read r :: Int) x) ys in
	rdGrps' p2 xs ys2

mrg :: Prison -> [Int] -> [Set Int] -> (Prison, [Set Int])
mrg p [a,b] ys
	| p_a && p_b = 
			let p2 = delete b $ delete a p in
			(p2, (fromList [a,b]):ys)
	| p_a = 
			let p2 = delete a p in
			let (truck,trucks) = pull b ys in
			(p2, (insert a truck):trucks)
	| p_b = 
			let p2 = delete b p in
			let (truck,trucks) = pull a ys in
			(p2, (insert b truck):trucks)
	| otherwise = 
			let (a_truck, a_trucks) = pull a ys in
			let (b_truck, ab_trucks) = pull b a_trucks in
			(p, (union a_truck b_truck):ab_trucks)
	where 
		p_a = member a p
		p_b = member b p

pull :: Int -> [Set Int] -> (Set Int, [Set Int])
pull x [] = (singleton x, [])
pull x (y:ys)
	| member x y = (y, ys)
	| otherwise = 
		let (ret,rest) = pull x ys in
		(ret,y:rest)
