import Data.List
import Debug.Trace

type Crossword = [[CCell]]
type CWord a = [a]
data WordList a = WordList Int [CWord a] deriving Show
data Solvable = Solvable [CCell] [CWord Char] deriving Show
data Point = Point Int Int deriving (Show, Eq)
data CCell = Filled | Empty Point | CellFill CCellFill deriving Show
data CCellFill = Solved Char Point deriving Show

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
parseRow [] y
	| length y > 1 = [y]
	| otherwise = []
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

cmpLens :: WordList a -> WordList a -> Ordering
cmpLens (WordList a _) (WordList b _) = compare a b

cmpSolv :: Solvable -> Solvable -> Ordering
cmpSolv (Solvable _ a) (Solvable _ b) = compare (length a) (length b)

wordFits :: [CCell] -> [Char] -> Bool
wordFits [] _ = True
wordFits (c:cs) (sc:str) =
	case c of
		Empty _ -> wordFits cs str
		CellFill (Solved c _) -> 
				if c == sc then wordFits cs str else False

removeWord :: [Char] -> [Solvable] -> [Solvable]
removeWord _ [] = []
removeWord str ((Solvable x vs):xs) =
	(Solvable x $ filter (/=str) vs):removeWord str xs

unifyCell :: CCellFill -> CCell -> CCell 
unifyCell _ cell@(CellFill (Solved _c _p)) = cell
unifyCell s@(Solved c p) cell@(Empty p2)
	| p == p2 = CellFill s
	| otherwise = cell

unifyWord :: [CCellFill] -> [CCell] -> [CCell]
unifyWord slvd [] = []
unifyWord slvd (x:xs) = foldl (\x y -> unifyCell y x) x slvd:unifyWord slvd xs

unifyWordSolvable :: [CCellFill] -> Solvable -> Solvable
unifyWordSolvable slvd (Solvable cells vals) = Solvable (unifyWord slvd cells) vals

unifyWordPoints :: [CCellFill] -> [Solvable] -> [Solvable]
unifyWordPoints slvd es = map (unifyWordSolvable slvd) es

solveWord :: [CCell] -> [Char] -> [CCellFill]
solveWord [] [] = []
solveWord [] _ = []
solveWord ((Empty p):xs) (c:cs) = Solved c p:solveWord xs cs
solveWord ((CellFill s@(Solved _ _)):xs) (c:cs) = s:solveWord xs cs

solvePuzzle :: [Solvable] -> Maybe [[CCellFill]]
solvePuzzle [] = Just []
solvePuzzle ((Solvable cells []):ss) = Nothing
solvePuzzle ((Solvable cells values):ss) =
	let v = head values in
	if wordFits cells v then
		let 
				c_solved = solveWord cells v;
				solvRest = unifyWordPoints c_solved $ removeWord v ss;
				solvedRest = solvePuzzle solvRest in
		case solvedRest of
			Nothing  -> solvePuzzle $ (Solvable cells $ tail values):ss
			Just ans -> Just (c_solved:ans)
	else 
		solvePuzzle $ (Solvable cells $ tail values):ss

createSolvables :: [WordList CCell] -> [WordList Char] -> [Solvable]
createSolvables [] [] = []
createSolvables ((WordList _ x):xs) ((WordList _ y):ys) =
	map (\gap -> Solvable gap y) x ++ createSolvables xs ys

lookupPoint :: Point -> [CCellFill] -> Char
lookupPoint _ [] = '+'
lookupPoint p (Solved c p2:cs)
	| p == p2 = c
	| otherwise = lookupPoint p cs

showSolvedLine _ [] _ = []
showSolvedLine l (w:ws)  solvables =
	c:showSolvedLine l ws solvables
	where c = lookupPoint (Point w l) solvables
	
showSolved [] _ _ = []
showSolved (l:ls) w solvables =
	(showSolvedLine l w solvables):showSolved ls w solvables

main = do
	cw <- getCrossword 10
	--putStrLn "~~~~~~~~~"
	--printCw cw
	--putStrLn "~~~~~~~~~"
	let horz = parseCwWords cw

	--putStrLn $ (++) "\n" $ show $ length horz
	--putStrLn "~~~~~~~~~"
	
	cw_words <- getWords
	--putStrLn $ show cw_words

	let wl_words = sortBy cmpLens $ partitionWords cw_words [];
			wl_empty = sortBy cmpLens $ partitionWords horz []
	--putStrLn . show $ wl_words
	--putStrLn . show $ wl_empty
	--putStrLn . show $ length $ partitionWords horz []

	--putStrLn "~~~~~~~~~"

	let solvables =  sortBy cmpSolv $ createSolvables wl_empty wl_words
	--putStrLn $ show solvables

	--putStrLn "~~~~~~~~~"

	let (Just solved) = solvePuzzle solvables
	--putStrLn . show $ solvePuzzle solvables

	--putStrLn "~~~~~~~~~"

	mapM_ putStrLn $ showSolved [0..9] [0..9] $ foldl (++) [] solved

