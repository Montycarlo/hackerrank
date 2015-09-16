import Debug.Trace

data Tree = Leaf | BTree Int Tree Tree deriving Show

constrStem n
	| n == -1 = Leaf
	| otherwise = BTree n Leaf Leaf

getInputLine :: Int -> IO (Tree,Tree)
getInputLine n = do
	l <- getLine
	let [a,b] = map (\x-> read x::Int) $ words l
	return (constrStem a, constrStem b)

getInputs :: Int -> IO [(Tree,Tree)]
getInputs n = mapM getInputLine [1..n]

countInputs c (Leaf, Leaf) = c
countInputs c (Leaf, _) = c+1
countInputs c (_, Leaf) = c+1
countInputs c (_, _) = c+2

appendTreeAtDepth :: Int -> Int -> Tree -> [(Tree,Tree)] -> ([(Tree, Tree)], Tree)
appendTreeAtDepth _ _ Leaf newVals = (newVals, Leaf)
appendTreeAtDepth d targD t@(BTree v Leaf Leaf) vals@((l, r):rest)
	| d == targD = 
			(rest, BTree v l r)
appendTreeAtDepth d targD (BTree v c_l c_r) vals
	| otherwise = 
			let (vals2, newL) = appendTreeAtDepth (d+1) targD c_l vals;
					(vals3, newR) = appendTreeAtDepth (d+1) targD c_r vals2 in
			(vals3, BTree v newL newR)
				

readTree :: Int -> Int -> Tree -> IO Tree
readTree 0 _ acc = return acc
readTree chldCnt d acc = do
	inps <- getInputs chldCnt
	let nextChildCnt = foldl countInputs 0 inps;
			(_, newTree) = appendTreeAtDepth 1 d acc inps
	readTree nextChildCnt (d+1) newTree
	
invertTree :: Tree -> [Int]
invertTree Leaf = []
invertTree (BTree v l r) = invertTree l ++ (v:invertTree r)

swapTree' :: Tree -> Int -> Int -> Tree
swapTree' Leaf _ _ = Leaf
swapTree' (BTree v l r) k_c k_k
	| rem k_c k_k == 0 = BTree v r_2 l_2
	| otherwise				 = BTree v l_2 r_2
	where 
			l_2 = swapTree' l (k_c+1) k_k;
			r_2 = swapTree' r (k_c+1) k_k;

swapTree :: Tree -> Int -> Tree
swapTree tree k = swapTree' tree 1 k

doSwaps 0 _ = return ()
doSwaps n tree = do
	k <- readLn :: IO Int
	let newTree = swapTree tree k
	putStrLn . unwords . map show $ invertTree newTree
	doSwaps (n-1) newTree

main = do
	n <- readLn :: IO Int
	tree <- readTree 1 1 (BTree 1 Leaf Leaf)

	t <- readLn :: IO Int
	doSwaps t tree
