import Control.Monad
import Data.List


-- transpose [[[]]
type Cell = (String, Int, Int)
type SudokuRow = [Cell]
type Sudoku = [SudokuRow]

type Count = (Cell, Int)

-- ASSUMPTION: there can nly be one duplicate per row
inCellArray :: Cell -> [Cell] -> [Cell]
inCellArray _ [] = []
inCellArray a@(y, _, _) (b@(x, _, _):xs)
	| x == y = [a,b]
	| otherwise = inCellArray a xs

--findDupesRow SearchRow -> FoundElements -> Duplicate Cells
findDupesRow :: SudokuRow -> [Cell] -> [Cell]
findDupesRow [] _ = []
findDupesRow (x:xs) fs = (inCellArray x fs) ++ (findDupesRow xs (x:fs))

findDupesRows :: Sudoku -> [Cell]
findDupesRows [] = []
findDupesRows (x:xs) = (findDupesRow x []) ++ (findDupesRows xs)

groupn :: Int -> [a] -> [[a]]
groupn _ [] = []
groupn n l
  | n > 0 = (take n l) : (groupn n (drop n l))
	| otherwise = error "Negative n"

combineRowsets :: [[[Cell]]] -> [[Cell]]
combineRowsets [] = []
combineRowsets rs@(z:zs)
	| length z == 0 = []
	| otherwise = (foldl (\x y -> x ++ y) [] $ map head rs):combineRowsets (map tail rs)

-- Groups each row into a set of 3 row blocks
groupColumns :: [[Cell]] -> [[Cell]]
groupColumns rows = combineRowsets $ map (groupn 3) rows

getSquares :: Sudoku -> [[Cell]]
getSquares s = foldl (++) [] $ map groupColumns $ groupn 3 s

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

countOccs :: [Cell] -> [Count]
countOccs [] = []
countOccs (x:xs) = (x, count x (x:xs)):(countOccs $ filter (\y -> x/=y) xs)

occsMax :: [Count] -> Int -> Int
occsMax [] m = m
occsMax ((_,n):cs) m = occsMax cs x
		where x = if m>n then m else n

findCommonDups :: [Cell] -> [Cell]
findCommonDups xs = 
		let cs = countOccs xs in
		let mx = occsMax cs 0 in
		map (\(x,_) -> x) $ filter (\(_,x) -> x == mx) cs

findDupes :: Sudoku -> [Cell]
findDupes s = 
		let d1 = findDupesRows s in
		let d2 = findDupesRows (transpose s) in 
		let sqrDups = findDupesRows $ getSquares s in
		let ds = d1 ++ d2 ++ sqrDups in
		findCommonDups ds

swapCellsRow :: Cell -> Cell -> SudokuRow -> SudokuRow
swapCellsRow _ _ [] = []
swapCellsRow a b (x:xs)
	| a == x		= b:rest
	| b == x		= a:rest
	| otherwise = x:rest
		where rest = swapCellsRow a b xs

cmpCells :: (Cell, Cell) -> (Cell, Cell) -> Ordering
cmpCells ((_,x1,y1), (_,x2,y2)) ((_,x3,y3), (_,x4,y4))
		| y1 < y2 = LT
		| y1 > y2 = GT
		| x1 < x2 = LT
		| x1 > x2  = GT 
		| otherwise = EQ


swapCells :: Sudoku -> Cell -> Cell -> Sudoku
swapCells s ca cb = map (swapCellsRow ca cb) s

isSudValid :: Sudoku -> Bool
isSudValid s = 0 == (length $ findDupes s)

findValidSwaps_ :: Sudoku -> Cell -> [Cell] -> [(Cell, Cell)]
findValidSwaps_ s c [] = []
findValidSwaps_ s c (y:ys) = 
		let sud_s = swapCells s c y in
		let valid = isSudValid sud_s in
		if valid then 
			(c, y):findValidSwaps_ s c ys
		else findValidSwaps_ s c ys
		

findValidSwaps :: Sudoku -> [Cell] -> [(Cell, Cell)]
findValidSwaps s []  = []
findValidSwaps s (x:xs) = (findValidSwaps_ s x xs) ++ (findValidSwaps s xs)

findSols :: Sudoku -> [(Cell, Cell)]
findSols s = 
		let dups = findDupes s in
		findValidSwaps s dups

showCell (s,x,y) = "("++s++", "++show y++", "++show x++")"
printSudoku x = putStr $ foldl (++) "" $ map (\y -> unwords (map showCell y) ++ "\n") x

cnvrtSukuRow_ ::  [String] -> (Int, Int) -> [Cell]
cnvrtSukuRow_ [] _ = []
cnvrtSukuRow_ (c:cs) (x, y) = (c, x, y):cnvrtSukuRow_ cs ((x+1), y)

cnvrtSukuRow :: [String] -> Int -> [Cell]
cnvrtSukuRow cs y = cnvrtSukuRow_ cs (1, y)

cnvrtSuku :: [[String]] -> Int -> [[Cell]]
cnvrtSuku [] _ = []
cnvrtSuku (x:xs) i = (cnvrtSukuRow x i):cnvrtSuku xs (i+1)

readSudokuFull = do
	lines <- replicateM 9 $ do 
		l <- getLine
		return l
	return $ map words lines

presentSwap :: (Cell, Cell) -> (Cell, Cell)
presentSwap (a@(_,x1,y1), b@(_,x2,y2))
		| y1 < y2 = (a, b)
		| y1 > y2 = (b, a)
		| x1 < x2 = (a, b)
		| otherwise = (b, a)

printCellPos :: Cell -> String
printCellPos (_,x,y) = "("++show y++","++show x++")"

printSwap :: (Cell, Cell) -> IO()
printSwap (a,b) = do
	putStrLn $ printCellPos a ++ " <-> " ++ printCellPos b

printSols_ :: [(Cell, Cell)] -> IO()
printSols_ [] = return ()
printSols_ (x:xs) = do
		printSwap $ presentSwap x	
		printSols_ xs

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
			lesser  = filter (< p) xs
			greater = filter (>= p) xs

printSols :: [(Cell, Cell)] -> IO()
printSols [] = putStrLn "Serendipity"
printSols xs = printSols_ $ sortBy cmpCells xs
	
processCase n maxN
	| n > maxN = return () 
	| otherwise = do
			sudoku <- readSudokuFull
			let sudoku_cv = cnvrtSuku sudoku 1
			putStrLn $ "Case #" ++ show n++":"
			printSols $ findSols sudoku_cv
			processCase (n+1) maxN

main = do
	caseStr <- getLine 
	let caseCount = read caseStr :: Int
	processCase 1 caseCount
