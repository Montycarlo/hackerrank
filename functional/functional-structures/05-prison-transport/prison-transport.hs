import Data.Set (Set, singleton, union, member, size, fromList, delete, insert, empty)
import qualified Data.Map.Strict as Map

data Prisoner = Prisoner{ prisonerId :: Int, truck :: Maybe Int } deriving Show
instance Eq Prisoner where
	x == y = (prisonerId x) == (prisonerId y)
instance Ord Prisoner where
	compare p1 p2 = compare (id p1) (id p2)

data Truck = Truck{ truckId :: Int, prisoners :: [Prisoner] } deriving Show
instance Eq Truck where
	x == y = (truckId x) == (truckId y)
instance Ord Truck where
	compare p1 p2 = compare (truckId p1) (truckId p2)

type Prison = Map.Map Int Prisoner

main = (readLn :: IO Int) >>= \n -> (readLn :: IO Int) >>= ferry n

ferry n m = do
	lnks <- mapM (\_->getLine) [1..m]
	let 
		prison = initPrison n
		trucks = Map.elems $ rdGrps prison $ map (\x->map read (words x)) lnks
		singles = n - (sum $ map (\t-> length $ prisoners t) trucks)
	putStrLn . show $ (+) singles $ sum $ map (\t-> cost $ prisoners t) trucks


initPrison n = Map.fromList $ zip [1..n] $ map (\x -> Prisoner { prisonerId=x, truck=Nothing }) [1..n]

cost :: [Prisoner] -> Int
cost g = ceiling . sqrt . fromIntegral $ length g

rdGrps :: Prison -> [[Int]] -> Map.Map Int Truck
rdGrps p xs = rdGrps' p xs Map.empty 1

rdGrps' :: Prison -> [[Int]] -> Map.Map Int Truck -> Int -> Map.Map Int Truck
rdGrps' _ [] ts _ = ts
rdGrps' p (x:xs) ts tid =
	let
		(p1, p2) = extr p x
		mgts@(truck1, truck2) = (getTruck ts p1, getTruck ts p2) in
	case mgts of
		(Nothing, Nothing) ->
				let 
					(p', ps') = updateTruckIds tid p [p1,p2]
					newt = Truck { truckId = tid, prisoners=ps' }
				in
					rdGrps' p' xs (Map.insert tid newt ts) (tid+1)
		(Just l, Nothing) ->
				let
					(p', [p2']) = updateTruckIds (truckId l) p [p2]
					newt = Truck { truckId=truckId l , prisoners=(p2':prisoners l) } 
				in
					rdGrps' p' xs (Map.insert (truckId l) newt ts) tid
		(Nothing, Just r) ->
				let
					(p', [p1']) = updateTruckIds (truckId r) p [p1]
					newt = Truck { truckId=truckId r , prisoners=(p1':prisoners r) }
				in
					rdGrps' p' xs (Map.insert (truckId r) newt ts) tid
		(Just l, Just r) ->
				if l == r then
					rdGrps' p xs ts tid
				else 
					let
						tid' = truckId l
						(p', pts') = updateTruckIds tid' p (prisoners r)
						newt = Truck { truckId=truckId l , prisoners=(pts' ++ prisoners l) }
					in
						rdGrps' p' xs (Map.insert tid' newt (Map.delete (truckId r) ts)) tid

extr p [p1,p2] = (p Map.! p1, p Map.! p2)
getTruck :: Map.Map Int Truck -> Prisoner -> Maybe Truck
getTruck ts p = truck p >>= (\x -> return $ ts Map.! x)

updateTruckIds _ prisoners [] = (prisoners, [])
updateTruckIds tid prisoners (p:ps) =
	let 
		p' = Prisoner{prisonerId=prisonerId p, truck=Just tid}
		(prisoners', rest) = updateTruckIds tid (Map.insert (prisonerId p) p' prisoners) ps
	in
	(prisoners', p':rest)
