import Control.Monad
import Data.List

getDiag :: [[Int]] -> Int -> [Int]
getDiag [] _ = []
getDiag (x:xs) n = (x!!n) : getDiag xs (n+1)

main = do
	n <- readLn :: IO Int 
	matrix_str <- replicateM n getLine

	let 
		matrix = map (map read . words) $ matrix_str
		f = \x -> sum $ getDiag x 0;
		ld = f matrix;
		rd = f $ map reverse matrix

	putStrLn.show.abs$ ld-rd

