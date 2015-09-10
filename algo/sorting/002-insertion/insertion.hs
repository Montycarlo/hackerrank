-- Note to reader: insertion sort in haskell is not pretty,
-- due do the nondestructive functional behaviour.
-- Especially in this challenge, where insertion is done 
-- from the right.

showArray xs = putStrLn . unwords $ map show xs

isort_ :: (Ord a, Eq a) =>  a -> [a] -> [a] -> [a]
isort_ n [] arr = n:arr
isort_ n arl arr
	| before <= n = arl ++ (n:arr)
	| otherwise  = isort_ n (dropl arl) (before:arr)
	where before = last arl

isort xs [] = return ()
isort xs (i:is) = do
	let xs_2 = isort_ i xs []
	showArray $ xs_2 ++ is
	isort xs_2 is

dropl [] = []
dropl [x] = []
dropl (x:xs) = x:dropl xs

setLast :: a -> [a] -> [a]
setLast x [_] = [x]
setLast x (y:ys) = y:setLast y ys

main = do
	s <- readLn :: IO Int
	ar_s <- getLine
	let ar = map (\x -> read x :: Int) $ words ar_s;
			xs_l = [head ar];
			unsorted = tail ar
	
	isort xs_l unsorted 
