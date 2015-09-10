-- Note to reader: insertion sort in haskell is not pretty,
-- due do the nondestructive functional behaviour.
-- Especially in this challenge, where insertion is done 
-- from the right.

showArray xs = putStrLn . unwords $ map show xs

isort :: (Ord a, Eq a, Show a) =>  a -> [a] -> [a] -> IO () --[a]
isort n [] arr = do
			showArray $ n:arr
isort n arl arr
	| before <= n = do
			let r = arl ++ (n:arr)
			showArray r
	| otherwise  = do
			showArray $ arl ++ (before:arr)
			isort n (dropl arl) (before:arr)
	where before = last arl

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
			n = last ar;
			ar_ = dropl ar
	
	isort n ar_ []
