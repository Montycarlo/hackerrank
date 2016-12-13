import Data.Char
main = (readLn :: IO Int) >>= \n -> mapM_ prank [1..n]

prank _ = getLine >>= putStrLn . show . prank'

prank' ss = countChanges ss rev (floor $ (fromIntegral len)/2.0)
	where 
		rev = reverse ss
		len = length ss

countChanges _ _ 0 = 0
countChanges (x:xs) (y:ys) n
	| ordx /= ordy = (abs $ ordx - ordy) + rest
	| otherwise = rest
	where 
		ordx = ord x
		ordy = ord y
		rest = countChanges xs ys (n-1)
