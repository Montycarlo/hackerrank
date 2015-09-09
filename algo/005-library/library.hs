
calc :: Int -> Int -> Int -> Int -> Int -> Int -> Int
calc rD rM rY dD dM dY
	| rY < dY = 0
	| rY > dY = 10000
	| rM > dM = 500 * (rM - dM)
	| rM < dM = 0
	| rD > dD = 15 * (rD-dD)
	| otherwise = 0

f_extr = map (\x -> read x :: Int) . words

main = do
	line_ret <- getLine	
	line_due <- getLine

	let [r_D, r_M, r_Y] = f_extr line_ret;
			[d_D, d_M, d_Y] = f_extr line_due

	putStrLn .show $ calc r_D r_M r_Y d_D d_M d_Y
