
type NVal = (Int, Int)
data NTree = Tree NVal NTree NTree | Leaf

updateLeaf :: Int -> NTree -> NTree
updateLeaf x Leaf = Tree (x, 1) Leaf Leaf
updateLeaf x (Tree y@(ya,yc) left right)
	| x == ya = Tree (ya,yc+1) left right
	| x < ya = Tree y (updateLeaf x left) right
	| otherwise = Tree y left (updateLeaf x right)

findLeaf :: NVal -> NTree -> Maybe NVal
findLeaf _ Leaf = Nothing
findLeaf needle@(x,_) (Tree l@(y,_) left right)
	| x == y = Just l 
	| x < y = findLeaf needle left
	| otherwise = findLeaf needle right

line2ints = map (\x -> read x :: Int) . words

testDiff :: NVal -> Maybe NVal -> IO()
testDiff (x1,c1) Nothing = putStr $ " " ++ show x1
testDiff (x1,c1) (Just (x2,c2))
		| c1 > c2 = putStr $ show x1 ++ " "
		| otherwise = return () 

-- aTree -> bTree
printDiffs :: NTree -> NTree -> IO ()
printDiffs _ Leaf = return ()
printDiffs aTree bTree@(Tree val left right) = do
	printDiffs aTree left
	testDiff val $ findLeaf val aTree
	printDiffs aTree right

main = do
	n <- getLine
	as <- getLine
	m <- getLine
	bs <- getLine
	let aTree = foldl (\x y -> updateLeaf y x) Leaf $ line2ints as
	let bTree = foldl (\x y -> updateLeaf y x) Leaf $ line2ints bs
	printDiffs aTree bTree
	
