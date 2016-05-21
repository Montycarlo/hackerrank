main = do
	n <- getLine
	w <- getLine
	putStrLn . unwords . reverse $ words w
