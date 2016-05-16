
main = do
	mc <- readLn :: IO Double
	tip_p <- readLn :: IO Double
	tax_p <- readLn :: IO Double
	let totalCost = round $ mc + tip + tax 
		where 
			tip = (mc * tip_p / 100)
			tax = (mc * tax_p / 100)
	putStrLn $ "The total meal cost is " ++ show totalCost ++ " dollars."
