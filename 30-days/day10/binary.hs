import Text.Printf

dropZeroes [] = []
dropZeroes _xs@(x:xs)
	| x == '1' = _xs
	| otherwise = dropZeroes xs

countConsec1s' :: Int -> String -> (String, Int)
countConsec1s' n [] = ([], n)
countConsec1s' n (x:xs)
	| x == '1' = countConsec1s' (n+1) xs
	| otherwise = (dropZeroes xs, n)

countConsec1s :: String -> (String, Int)
countConsec1s [] = ([], 0)
countConsec1s xs = countConsec1s' 0 xs

consecs :: String -> [Int]
consecs [] = []
consecs ns =  count:consecs rest
	where (rest, count) = countConsec1s ns
	
main = do
	n <- readLn :: IO Int
	putStrLn . show . maximum . consecs $ printf "%b" n
