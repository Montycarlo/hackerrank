import Data.Char
calc xs hs =
  let hh = map (\x -> hs !! (ord x-97)) xs in
  (length xs) * maximum hh

main = do
  l <- getLine
  let hs = map (\x -> read x :: Int) $ words l
  xs <- getLine
  putStrLn . show $ calc xs hs

