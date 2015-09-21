import Debug.Trace
import Text.Printf (printf)

calc :: [Double] -> [Double] -> Double -> Double
calc as bs x = sum $ map (\(a,b) -> a * x ** b) $ zip as bs


-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = 
	let step = 0.001
			range = [fromIntegral l,((fromIntegral l)+step)..fromIntegral r];
			points = [calc (map fromIntegral a) (map fromIntegral b) x | x <- range];
			faces = map (\r -> pi * r**2) points in
	[step * sum points, step * sum faces]
	

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
	
