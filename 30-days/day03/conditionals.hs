
mapTheN :: Int -> String
mapTheN n
	| n `rem` 2 == 1 = "Weird"
	| n <= 5 && n >= 2 = "Not Weird"
	| n <= 20 && n >= 6 = "Weird"
	| otherwise = "Not Weird"

main = do
	n <- readLn :: IO Int
	putStrLn $ mapTheN n
