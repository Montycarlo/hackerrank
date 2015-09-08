
processCase [] red yellow green blue = 
	red == green && yellow == blue
processCase (x:xs) red yellow green blue 
	| x == 'Y' = processCaseResult xs red (yellow+1) green blue
	| x == 'G' = processCaseResult xs red yellow (green+1) blue
	| x == 'R' = processCaseResult xs (red+1) yellow green blue
	| x == 'B' = processCaseResult xs red yellow green (blue+1)

processCaseResult x red yellow green blue
	| abs (red - green) > 1 = False
	| abs (yellow - blue) > 1 = False
	| otherwise = processCase x red yellow green blue

processCases 0 = return ()
processCases n = do
	casen <- getLine	
	putStrLn . show $ (processCase casen 0 0 0 0)
	processCases (n-1)
	

main = do
		cases_str <- getLine
		processCases $ (read cases_str :: Int)
