
countBts' ns nextNum = 
		let pairs = [ [x,nextNum-x] | x <- [0..nextNum] ];
				combs = map (product . map (\x->ns !! x)) pairs;
				v = sum combs in
		v:countBts' (ns++[v]) (nextNum+1)
countBts = 1:1:2:countBts' [1,1,2] 2


doCases 0 = return ()
doCases t = do
	n <- readLn :: IO Int
	putStrLn $ show $ rem (countBts !! n) (10^8+7)
	doCases (t-1)

main = do
	t <- readLn :: IO Int
	doCases t

