
splitBST x [] acc = (acc, [])
splitBST x _y@(y:ys) acc
	| x > y = splitBST x ys (acc++[y]) 
	| otherwise = (acc, _y)

testValidBST [] = True
testValidBST (x:xs) =
	let (xs_l, xs_r) = splitBST x xs [] in
	if null $ filter (x>) xs_r then 
		testValidBST xs_l && testValidBST xs_r
	else
		False

doCases 0 = return ()
doCases t = do
	n <- readLn :: IO Int
	l <- getLine
	let ws = map (\x->read x::Int) $ words l;
			valid = testValidBST ws
	
	putStrLn $ if valid then "YES" else "NO"
	doCases (t-1)

main = do
	t <- readLn :: IO Int
	doCases t
