read N
sum=0
for (( i=0; i<$N; i++ ))
do
  read x
  let "sum += $x"
done
dec=$(echo "$sum / $N" | bc -l)
printf "%.3f" $dec 
