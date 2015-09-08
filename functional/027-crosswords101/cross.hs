import Data.List

type Crossword = [[CCell]]
data CCell = Filled | Empty | Word Char deriving Show

parseChar :: Char -> CCell
parseChar c
	| c == '-' = Empty
	| otherwise = Filled

getCrosswordLine :: Int -> IO [CCell]
getCrosswordLine n = do
	x <- getLine
	return $ map parseChar x

getCrossword n = mapM getCrosswordLine [1..n]

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
	Empty -> parseRow xs (y++[x])

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

getWords :: [String]
getWords = do
	l <- getLine 
	wordsWhen (==';') l

main = do
	cw <- getCrossword 10
	putStrLn "~~~~~~~~~"
	printCw cw
	putStrLn "~~~~~~~~~"
	let horz = parseCwWords cw
	putStrLn $ show $ length horz
	
	cw_words <- getWords
	show.putStrLn cw_words
