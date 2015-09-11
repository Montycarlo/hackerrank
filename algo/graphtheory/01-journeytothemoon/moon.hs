import qualified Data.Set as S
import Debug.Trace

data NTree = Tree Int NTree NTree | Leaf deriving Show

searchTree :: NTree -> Int -> Maybe NTree
searchTree Leaf _ = Nothing
searchTree t@(Tree v l r) s
	| v == s = Just t
	| v < s = searchTree r s
	| otherwise = searchTree l s

flattenTree :: [NTree] -> [Int]
flattenTree []		 = []
flattenTree (x:xs) =
	case x of
		Leaf				 -> flattenTree xs
		(Tree v l r) -> v:(flattenTree $ xs++[l,r])

mergeTrees :: NTree -> NTree -> NTree
mergeTrees a b =
	let flat_b = flattenTree [b] in
	foldl addToTree a flat_b

addToTree :: NTree -> Int -> NTree
addToTree Leaf x = Tree x Leaf Leaf
addToTree t@(Tree v l r) x
	| v == x = t
	| v < x = Tree v l (addToTree r x)
	| otherwise = Tree v (addToTree l x) r

addNsToTree :: NTree -> [Int] -> NTree
addNsToTree t xs = foldl (\_t x -> addToTree _t x) t xs

anyInTree :: NTree -> [Int] -> Bool
anyInTree tree [] = False
anyInTree tree (x:xs) = 
	case sr of
		Nothing -> anyInTree tree xs
		otherwise -> True
	where sr = searchTree tree x

searchAndMerge :: NTree -> [Int] -> (Bool, NTree)
searchAndMerge tree xs
	| within = (True, addNsToTree tree xs)
	| otherwise = (False, tree)
	where within = anyInTree tree xs
	
extractTrees :: [NTree] -> [Int] -> NTree -> [NTree]
extractTrees [] as accum = [accum]
extractTrees (t:ts) as _ts =
	let (merged, t_n) = searchAndMerge t as in
	if merged then
		let _tsn = mergeTrees t_n _ts in
		extractTrees ts as _tsn
	else
		t:extractTrees ts as _ts

readNations :: [NTree] -> Int -> IO [NTree]
readNations trees 0 = return trees
readNations trees n = do
		l <- getLine
		let as = map (\x-> read x::Int) $ words l;
				trees_n = extractTrees trees as $ addNsToTree Leaf as
		readNations trees_n (n-1)

countElements :: NTree -> Int
countElements Leaf = 0
countElements (Tree _ l r) = 1 + countElements l + countElements r

countTeamPairs :: Int -> Int -> Int
countTeamPairs x y = x * y

countPermutations :: [Int] -> Int
countPermutations [] = 0
countPermutations (x:ys) = 
	let p_x = foldl (\a y -> a+ countTeamPairs x y) 0 ys in
	p_x + countPermutations ys

countPermutations_tree :: [NTree] -> Int
countPermutations_tree xs = countPermutations $ map countElements xs

main = do
	fl <- getLine
	let [n,i] = map (\x->read x::Int) $ words fl;
			astronauts = [Tree i Leaf Leaf| i <- [0..(n-1)]];
	
	nations <- readNations astronauts i
	putStrLn "~~~~~~~"
	--mapM_ (putStrLn . show) nations
	putStrLn . show $ length nations
	putStrLn "~~~~~~~"
	putStrLn . show $ countPermutations_tree nations
	
