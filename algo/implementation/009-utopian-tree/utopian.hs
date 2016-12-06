
readIntLn :: IO Int
readIntLn = getLine >>= \x -> return $ read x

main = do
  t <- readIntLn
  mapM_ doCases [0..t]

doCases _ = do
  n <- readIntLn
  putStrLn . show $ spring n 1

spring n acc
  | n <= 0 = acc
  | otherwise = summer (n-1) (acc*2)

summer n acc
  | n <= 0 = acc
  | otherwise = spring (n-1) (acc+1)
