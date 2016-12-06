
main = getLine >>= \l -> putStrLn $ cvrt $ reduceStr l []
cvrt [] = "Empty String"
cvrt xs = xs
reduceStr [] ys = reverse ys
reduceStr (x:xs) [] = reduceStr xs [x]
reduceStr (x:xs) (y:ys)
	| x == y = reduceStr xs ys
	| otherwise = reduceStr xs (x:y:ys)

