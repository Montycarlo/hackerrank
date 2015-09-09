

main = do
	n <- getLine
	ns <- getLine
	putStrLn . show . sum . map read . words $ ns
