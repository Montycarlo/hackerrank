pascalNext b [] = [b]
pascalNext b (x:xs)
	= (b+x):pascalNext x xs

pascal' xs = 
	n : pascal' n
	where n = pascalNext 0 xs
pascal = [1]:pascal' [1]

doCases 0 = return ()
doCases t = do
	l <- getLine
	let [n,k] = map (\x->read x::Int) $ words l
	putStrLn . show $ rem (pascal !! n !! k) (10^8+7)
	doCases (t-1)

main = do
	t <- readLn :: IO Int
	doCases t
