import qualified Data.Map as Map

grabMapEntry _ = do
	l <- getLine
	let [a,b] = words l
	return (a,b)

showEntry x Nothing = "Not found"
showEntry x (Just y) = x++"="++y

query :: (Map.Map String String) -> IO()
query m = do
	name <- getLine
	let l = Map.lookup name m
	putStrLn $ showEntry name l
	query m

main = do
	n <- readLn :: IO Int
	m_e <-  mapM grabMapEntry [1..n]
	let m = Map.fromList m_e
	query m
