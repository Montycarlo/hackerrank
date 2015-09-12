import Data.List

type Crossword = [[CCell]]
type CWord a = [a]-- [CCell]
data WordList a = WordList Int [CWord a] deriving Show
data Point = Point Int Int deriving Show
data CCell = Filled | Empty Point deriving Show

parseChar :: (Point, Char) -> CCell
parseChar (p, c)
	| c == '-' = Empty p
	| otherwise = Filled

getCrosswordLine :: Int -> IO [CCell]
getCrosswordLine n = do
	x <- getLine
	let ps = map (\x-> Point x n) [0..(length x)]
	return $ map parseChar $ zip ps x

getCrossword n = mapM getCrosswordLine [0..(n-1)]

printCw :: [[CCell]] -> IO()
printCw [] = return ()
printCw cs = do
	putStrLn $ foldl (\x y -> x ++ show y ++ "\n") [] cs

parseRow :: [CCell] -> [CCell] -> [[CCell]]
parseRow [] [] = []
parseRow [] y = [y]
parseRow (x:xs) y = case x of
	Filled -> if length y > 1 then y:rest else rest
						where rest = parseRow xs []
	Empty _ -> parseRow xs (y++[x])

-- Parse a given Crossword, and extract a list of empty consequtive
-- cells, flip the crossword and then do the same, concat the two results
parseCwWords :: Crossword -> [[CCell]]
parseCwWords [] = []
parseCwWords rs = horz ++ vert
	where	
		conds = foldl (\x y -> x ++ parseRow y []) []
		horz = conds rs
		vert = conds $ transpose rs

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
	"" -> []
	s' -> w : wordsWhen p s''
				where (w, s'') = break p s'

getWords :: IO [String]
getWords = do
	l <- getLine 
	return $ wordsWhen (==';') l

addWordToPartitions :: CWord a -> [WordList a] -> [WordList a]
addWordToPartitions w [] = [WordList (length w) [w]]
addWordToPartitions w (x@(WordList x_n x_wl):xs)
	| length w == x_n = WordList x_n (w:x_wl):xs
	| otherwise = x:addWordToPartitions w xs

partitionWords :: [CWord a] -> [WordList a] -> [WordList a]
partitionWords [] acc = acc
partitionWords (x:xs) acc = 
	partitionWords xs $ addWordToPartitions x acc

main = do
	cw <- getCrossword 10
	putStrLn "~~~~~~~~~"
	printCw cw
	putStrLn "~~~~~~~~~"
	let horz = parseCwWords cw
	putStrLn $ show $ length horz
	
	cw_words <- getWords
	putStrLn $ show cw_words
	putStrLn . show $ partitionWords cw_words []
	putStrLn . show $ partitionWords horz []
	putStrLn . show $ length $ partitionWords horz []
