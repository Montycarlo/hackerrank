import Data.Array.IO
import Control.Monad

if' True x _ = x
if' False _ y = y

getList = getLine >> getLine >>= \ls -> return $ map (\r->read r::Int) $ words ls
main = do
  xs <- getList
  ys <- getList
  let first = xs!!0
  fs <- newArray (first-100, first+100) 0 :: IO (IOUArray Int Int)
  forM_ ys (inc fs)
  forM_ xs (dec fs)
  printPositives fs [first-100..first+100]

printPositives :: (IOUArray Int Int) -> [Int] -> IO()
printPositives _ [] = return ()
printPositives fs (x:xs) = do
  v <- readArray fs x
  if' (v>0) (putStr (show x ++ " ")) (return ())
  printPositives fs xs

inc arr i = do
  v <- readArray arr i
  writeArray arr i (v+1)
dec arr i = do
  v <- readArray arr i
  writeArray arr i (v-1)
