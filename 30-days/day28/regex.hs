import Control.Applicative
import Control.Monad
import System.IO
import Data.List

getWords = getLine >>= return.words

googleMail [_, s] = 
	(take (length x) $ reverse s) == (reverse x)
	where x = "@gmail.com"

main :: IO ()
main = do
		n <- getLine
		ns <- mapM (\x->getWords) [1..(read n::Int)]
		mapM_ putStrLn $ sort $ map (\[x, _] -> x) $ filter googleMail ns

