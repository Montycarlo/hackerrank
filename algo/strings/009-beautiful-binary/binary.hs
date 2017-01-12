
main = getLine >> getLine >>= putStrLn . show . countChanges

countChanges [] = 0
countChanges [_,_] = 0
countChanges "010" = 1
countChanges [_,_,_] = 0
countChanges (a:b:c:d:ds)
  | [a,b,c] /= "010" = countChanges (b:c:d:ds)
  | d == '1' = 1 + countChanges (b:'1':d:ds)
  | d == '0' = 1 + countChanges ('0':c:d:ds)
