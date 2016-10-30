import Data.Maybe
-- base, power
type Factor = (Int,Int)

convWs [] = []
convWs (x:y:xs) = (x,y):convWs xs

readFactors :: Int -> IO [Factor]
readFactors _ = do
	l <- getLine
	let fs = map (\x->read x::Int) $ words l
	return $ convWs fs	

findCommon :: Factor -> [Factor] -> Maybe Factor
findCommon x [] = Nothing
findCommon x@(x_a,x_b) ((y_a,y_b):ys)
	| x_a==y_a = Just (x_a,min x_b y_b)
	| otherwise = findCommon x ys

findSmallest :: [Factor] -> Factor
findSmallest [x] = x
findSmallest [(a_b,a_p), (b_b,b_p)] = (a_b, min a_p b_p)
findSmallest (x:xs) = findSmallest [x, findSmallest xs]

findCommonDivs' :: [Factor] -> [[Factor]] -> [Factor]
findCommonDivs' [] _ = []
findCommonDivs' (x:xs) ys = 
	let com = map (findCommon x) ys;
			any_null = any isNothing com in
	if any_null then
		rest
	else
		findSmallest (mapMaybe (\x->x) com):rest
	where rest = findCommonDivs' xs ys

findCommonDivs (x:xs) = findCommonDivs' x xs

showCommon (a,b) = show a ++ " " ++ show b

main = do
	q <- readLn :: IO Int
	factors <- mapM readFactors [1..q]
	let common = findCommonDivs factors

	putStrLn . unwords $ map showCommon common
