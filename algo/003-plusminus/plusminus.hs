import Numeric

formatFloatN numOfDecimals floatNum = showFFloat (Just numOfDecimals) floatNum ""

numOf :: (a -> Bool) -> [a] -> Int
numOf f xs = length $ filter f xs

main = do
	n <- getLine
	ns_str <- getLine
	let ns = map (\x -> read x :: Int) $ words ns_str;
			positive = numOf (>0) ns;
			zeros		 = numOf (==0) ns;
			negative = numOf (<0) ns;
			len_t = length ns;
			calc = \x -> formatFloatN 3 $ fromIntegral x / fromIntegral len_t


	putStrLn $ calc positive
	putStrLn $ calc negative  
	putStrLn $ calc zeros 
