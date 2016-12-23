import qualified Data.ByteString.Char8 as C8
import Data.List

readInts :: IO [Int]
readInts = C8.getLine >>= return . (foldr (\(Just (i,_)) as -> i:as) []) . (map C8.readInt) . C8.words

main = do
	[_,k] <- readInts
	xs2  <- readInts
	let xs = sort xs2
	putStrLn . show $ countRadios (tail xs) k (head xs) (head xs)

countRadios [] _ _ _ = 1
countRadios (x:xs) k ref last
	| x-ref > k = 1 + rest
	| otherwise = countRadios xs k ref x
	where 
		popped = popOff (x:xs) k last
		rest = case popped of
				[] -> 0
				_ -> countRadios (tail popped) k (head popped) (head popped)

popOff :: [Int] -> Int -> Int -> [Int]
popOff [] _ _ = []
popOff (x:xs) k ref
	| x-ref > k = x:xs
	| otherwise = popOff xs k ref
