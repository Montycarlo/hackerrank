import Control.Monad
import Data.Bits

findK' :: Int -> Int -> Int
findK' _ 0 = 0
findK' n k
	| n == k = findK (n-1) k
	| nk /= k = findK (n-1) k
	| otherwise = nk
	where nk = n .&. k

findK _ 0 = 0
findK n k
	| n == k = findK n (k-1)
	| nk == 0 = findK n (k-1)
	| otherwise = nk
	where nk = findK' n k

doCase _ = do
	raw <- getLine
	let [n, k] = map (\x->read x::Int) $ words raw
	putStrLn $ show $ findK n (k-1)

main = do
	t <- readLn :: IO Int
	forM_ [1..t] doCase
