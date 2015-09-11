d a b=(0==)$mod a b
fb n
	|t&&f=a++b
	|t=a
	|f=b
	|otherwise=show n
	where 
		t=d n 3;
		f=d n 5;
		a="Fizz";
		b="Buzz"
main = do
	putStrLn.unlines$map fb [1..100]
