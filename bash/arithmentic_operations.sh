read expression
x=$(echo "$expression" | bc -l)
printf "%.3f" $x
