import Data.List

main = do
  t <- readLn :: IO Int
  mapM_ doCase [1..t]

doCase _ = do
  n <- readLn :: IO Int
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  putStrLn . unwords . map show . sort . nub $ allPossibleValues (n-1,a) (0,b)

allPossibleValues (-1,_) (_,_) = []
allPossibleValues (a_n,a) (b_n,b) = (a*a_n+b*b_n):allPossibleValues (a_n-1,a) (b_n+1,b)
