main = do
    n <- getLine
    ns <- getLine
    putStrLn . unwords . reverse $ words ns
