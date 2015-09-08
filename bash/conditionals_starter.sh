read -n 1 c
if [ $c = 'y' ] || [ $c = 'Y' ]; then
  echo 'YES'
elif [ $c = 'n' ] || [ $c = 'N' ]; then
  echo 'NO'
fi 
