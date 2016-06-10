
parseDate = do
	l <- getLine
	return $ map (\x->read x::Int) $ words l

calcFine [rd,rm,ry] [dd,dm,dy]
	| ry < dy = 0
	| ry > dy = 10000
	| rm < dm = 0
	| rm > dm = 500*(rm-dm)
	| rd < dd = 0
	| rd > dd = 15*(rd-dd)
	| otherwise = 0

main = do
	d1 <- parseDate
	d2 <- parseDate
	putStrLn . show $ calcFine d1 d2
