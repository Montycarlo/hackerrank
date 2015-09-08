#! /bin/bash
read N
read -a array

for n in ${array[*]}
do
	count=$(for x in ${array[*]}; do printf "$x\n"; done | grep -c $n)
	if [ $count -eq 1 ]
	then
		echo $n
	fi	
done

