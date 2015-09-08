
type NVal = (Int, Int, Int)
data NTree = Tree NVal NTree NTree | Leaf

findN n i Leaf = (n, 1, i)
findN n i (Tree (a,b,ti) left right)
	| n == a = (a,b+1,ti)
	| n < a = findN n i left
	| otherwise = findN n i right 

replaceN :: NVal -> NTree -> NTree
replaceN x Leaf = Tree x Leaf Leaf
replaceN x@(n,_,_) (Tree y@(a,c,ti) left right)
	| n == a = Tree x left right
	| n < a = Tree y (replaceN x left) right
	| otherwise = Tree y left (replaceN x right) 

insert_sat a [] = [a]
insert_sat a@(aa,_,a_pos) (x@(xa,_,x_pos):xs)
	| aa == xa = x:xs
	| a_pos < x_pos = a:x:xs
	| otherwise = x:insert_sat a xs

-- unprocessed, cPos requiredCount, processingCount, Satisfied
find_sats :: [Int] -> Int -> Int -> NTree -> [NVal] -> [Int]
find_sats [] _ _ _ satisfied
	| length satisfied == 0 = [-1]
	| otherwise = map (\(a,_,_) -> a) satisfied

find_sats (x:xs) i k processing satisfied
	| b == k =
		let satisfied_new = insert_sat node satisfied in
		find_sats xs i_new k processing_new satisfied_new
	| otherwise = 
		find_sats xs i_new k processing_new satisfied
	where 
		node@(a,b,c) = findN x i processing
		processing_new = replaceN node processing
		i_new = i+1

	

doCases 0 = return ()
doCases t = do
    meta <- getLine
    let metaVal = words meta
    let n = read (metaVal !! 0) :: Int 
    let k = read (metaVal !! 1) :: Int 
    src <- getLine
    putStrLn . unwords $ map show $ find_sats (map (\x -> read x :: Int) $ words src) 1 k Leaf []
    doCases (t-1)

main = do
		t_str <- getLine
		doCases (read t_str :: Int)
