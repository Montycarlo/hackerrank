import Text.Printf

parseTime :: String -> (Int, Int, Int, Bool)
parseTime s = 
	let intv = \x -> read x :: Int;
			(hh, _rest) = splitAt 2 s;
			(mm, __rest) = splitAt 2 $ drop 1 _rest;
			(ss, apm) = splitAt 2 $ drop 1 __rest;
			pm = apm == "PM" in
	(intv hh, intv mm, intv ss,pm)

convertTime :: (Int, Int, Int, Bool) -> (Int, Int, Int)
convertTime (hh,mm,ss,pm)
	| pm == True = (hh + if hh < 12 then 12 else 0,mm,ss)
	| otherwise = (if hh == 12 then 0 else hh,mm,ss)

showTime :: (Int, Int, Int) -> IO ()
showTime time@(hh,mm,ss) = printf "%02d:%02d:%02d\n" hh mm ss :: IO ()

main = do
	l <- getLine
	showTime . convertTime $ parseTime l
