read x
read y
read z
if [ $x = $y ] && [ $y = $z ]; then
  echo 'EQUILATERAL'
elif [ $x -ne $y ] && [ $x -ne $y ] && [ $y -ne $z ]; then
  echo 'SCALENE'
else
  echo 'ISOSCELES'
fi
