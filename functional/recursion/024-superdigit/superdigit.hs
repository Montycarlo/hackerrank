
c_p :: String -> Int -> String 
c_p n 0 = ""
c_p n k = n ++ c_p n (k-1)

superdigit :: String -> Char
superdigit [x] = x
superdigit xs = 
	let _sum = sum $ map (\s -> read ([s]) :: Int) xs in
	superdigit $ show _sum

main = do
	line <- getLine
	let strs = words line
	let n = strs !! 0
	let k = read (strs !! 1) :: Int

	let p = c_p [superdigit n] k
	let super = superdigit p

	putStrLn [super]

