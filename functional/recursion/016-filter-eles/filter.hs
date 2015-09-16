verifyMinOccs :: Eq a => a -> [a] -> Int -> Int -> [a] -> (Bool, [a])
verifyMinOccs x [] acc n f = (acc >= n, reverse f)
verifyMinOccs x (h:hs) acc n f
		| x == h = verifyMinOccs x hs (acc+1) n f
		| otherwise = verifyMinOccs x hs acc n (h:f)

find_sats [] _ = []
find_sats (h:hs) k 
		| v = h:rest
		| otherwise = rest 
			where 
				(v, filtd) = verifyMinOccs h hs 1 k []
				rest = find_sats filtd k

addOne x [] = [(x,1)]
addOne x (pb@(p, pc):ps)
		|x == p = (p, pc+1):ps
		|otherwise = pb:addOne x ps 

find_counts [] ps = ps
find_counts (x:xs) ps = 
		let ps_new = addOne x ps in
		find_counts xs ps_new

rip_ns [] _ = []
rip_ns ((xa, xb):xs) k
		| xb >= k		= xa:rip_ns xs k
		| otherwise = rip_ns xs k

find_sats2 haystack k = 
		let ps = find_counts haystack [] in
		rip_ns ps k

doCases 0 = return ()
doCases t = do
		meta <- getLine
		let metaVal = words meta
		let n = read (metaVal !! 0) :: Int
		let k = read (metaVal !! 1) :: Int
		src <- getLine
		let haystack = words src
		let result = find_sats2 haystack k
		let passed = if length result > 0 then result else ["-1"]
		putStrLn . unwords $ passed
		doCases (t-1)

main = do
		tstr <- getLine
		doCases (read tstr :: Int)
