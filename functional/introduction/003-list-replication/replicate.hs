repl_ele 0 _ = []
repl_ele n x = (x:repl_ele (n-1) x)

f :: Int -> [Int] -> [Int]
f _ [] = []
f n (x:xs) = 
	(repl_ele n x) ++ (f n xs)
	

-- This part handles the Input and Output and can be used as it is. Do not modify this part.
main :: IO ()
main = getContents >>=
       mapM_ print. (\(n:arr) -> f n arr). map read. words

