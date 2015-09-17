import Data.List

type NVal = (Int, Int)
data NTree = Tree NVal NTree NTree | Leaf

findMinEles n Leaf = Nothing
findMinEles n (Tree z@(x,y) left right)
	| x == n = Just z
	| x < n = findMinEles n right
	| otherwise =
			let s = findMinEles n left in
			case s of
				Nothing -> Just z
				otherwise -> s 

extractCount Nothing = -1
extractCount (Just (_,c)) = c

doCases 0 _ = return ()
doCases n tree = do
	xstr <- getLine
	let x = read xstr :: Int
	putStrLn . show . extractCount $ findMinEles x tree
	doCases (n-1) tree


sums [] _ _ = []
sums (a:as) i sum = (sum_new, i):sums as (i+1) sum_new
		where sum_new = a+sum

createTreeNodeAt [] _ _ left = Leaf
createTreeNodeAt (a:as) x i left
	| x == i = 
		let 
			left_tree = (createTreeNodeAt (reverse left) ((length left) `quot` 2) 0 [])
			right_tree = (createTreeNodeAt as ((length as) `quot` 2) 0 [])
			in
				Tree a left_tree right_tree 
	| otherwise = createTreeNodeAt as x (i+1) (a:left)
	

createTree :: [NVal] -> NTree
createTree [] = Leaf
createTree as = createTreeNodeAt as ((length as) `quot` 2) 0 []

main = do
	nstr <- getLine
	astr <- getLine
	tstr <- getLine

	let as = createTree $ (\x -> sums x 1 0) $ sortBy (flip compare) $ map (\x -> read x :: Int) $ words astr
	let t = read tstr :: Int
	doCases t as
	

