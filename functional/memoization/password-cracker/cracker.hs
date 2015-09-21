removePrefix :: String -> String -> String
removePrefix [] rest = rest
removePrefix (x:xs) (y:ys)
	| x == y = removePrefix xs ys
	| otherwise = error "prefix invalid"

prefixOf :: String -> String -> Bool
prefixOf [] _ = True
prefixOf xs [] = False
prefixOf (x:xs) (y:ys)
	| x == y = prefixOf xs ys
	| otherwise = False

isValidUsingSubstrs :: [String] -> [String] -> String -> Maybe [String]
isValidUsingSubstrs _ [] _ = Nothing
isValidUsingSubstrs mastr (o:opts) rest = 
	let rest_ = removePrefix o rest;
			next = filter (\x->prefixOf x rest_) mastr in
	if rest_ == [] then Just [o]
	else
		if null next then
			isValidUsingSubstrs mastr opts rest
		else
			let result = isValidUsingSubstrs mastr next rest_ in
			case result of
				Nothing -> isValidUsingSubstrs mastr opts rest
				Just xs -> Just (o:xs)
	

charsInOpts :: String -> String -> Bool
charsInOpts _ [] = True
charsInOpts opts (s:str)
	| elem s opts = charsInOpts opts (filter (\x->x /= s) str)
	| otherwise		= False

isValid :: [String] -> String -> Maybe [String]
isValid substrs passwd = 
	if charsInOpts (foldl (++) [] substrs) passwd then
		let next = filter (\x->prefixOf x passwd) substrs in
		if null next then
			Nothing
		else
			isValidUsingSubstrs substrs next passwd
	else Nothing

	
doCases 0 = return ()
doCases t = do
	n <- readLn :: IO Int
	l <- getLine 
	p <- getLine
	let l_ws = words l;
			r = isValid l_ws p
	case r of
		Nothing -> putStrLn "WRONG PASSWORD"
		Just x -> putStrLn $ unwords x
	doCases (t-1)

main = do
	t <- readLn :: IO Int
	doCases t
