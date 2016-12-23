import qualified Data.ByteString.Char8 as C8
import Data.List

readInts :: IO [Int]
readInts = C8.getLine >>= return . (foldr (\(Just (i,_)) as -> i:as) []) . (map C8.readInt) . C8.words

main = readLn >>= \l -> mapM_ doCase [1..l]

doCase _ = do
	n <- readLn :: IO Int
	as <- readInts
	putStrLn . hasEqualPivot $ pivot as

hasEqualPivot Nothing = "NO"
hasEqualPivot _ = "YES"

pivot :: [Int] -> Maybe Int
pivot [a] = Just a
pivot (a:as) = pivot' a as (sum as)
pivot' acc (x:xs) xsum 
	| acc == sumxs = Just x
	| acc > sumxs = Nothing
	| otherwise = pivot' (acc+x) xs sumxs
	where sumxs = xsum-x
