#! /bin/bash
read n

# $1 is the number of elements in row
# $2 is the row iteration
function printDiagnolLine {
	num_between=$(((64/$1/2)-1))

	padding=$((100-$1/2-$num_between*(1+$1/2)))
	padding_left=17 #$(($padding/2))
	padding_right=$(($padding_left+($padding % 2)))
	
	for x in $(seq $padding_left); do printf "_"; done

	sum=$padding_left
	last_it=$(($1/2))
	for n in $(seq $last_it)
	do
		# calculate the center point
		center=$(((64/$last_it)-1))
		num_center=$((1+2*$2))
		out_pad=$((($center-$num_center)/2))
		for x in $(seq $out_pad); do printf "_"; done
		printf "1"
		for x in $(seq $num_center); do printf "_"; done
		printf "1"
		for x in $(seq $(($out_pad-1))); do printf "_"; done
		sum=$(($sum+$num_center+$out_pad*2+1))
	done

	for x in $(seq $padding_right); do printf "_"; done
	sum=$(($sum+$padding_right))

	for x in $(seq $((100-$sum))); do printf "_"; done
	printf "\n"
}

function printVerticalStreamLine {
	num_between=$(((64/$1)-1))

	padding=$((100-$1-$num_between*(1+$1)))
	padding_left=$(($padding/2))
	padding_right=$(($padding_left+($padding % 2)))
	
	if [ $1 -eq 1 ]; then 
		num_between=0;
		padding_left=49; 
		padding_right=50; 
	fi

	for x in $(seq $padding_left); do printf "_"; done

	for n in $(seq $1)
	do
		for x in $(seq $num_between); do printf "_"; done
		printf "1"
	done
	for x in $(seq $(($num_between))); do printf "_"; done
	for x in $(seq $padding_right); do printf "_"; done

	printf "\n"
}

function printBlank {
	for x in $(seq 100); do printf "_"; done
	printf "\n"
}

function printSegment {

	if [ $1 -ne 64 ]; then
		num_=$(((64/$1)/4))
		for n in $(seq $num_); do 
			if [ $3 -ge $(($1)) ]
		 	then
				printDiagnolLine $1*2 $(($num_-$n));
			else
				printBlank;
			fi
		done;
	fi

	num_=$(((64/$1)/4))
	if [ $1 -eq 64 ]; then num_=1; fi
	for n in $(seq $num_); do 
		if [ $3 -ge $1 ] 
		then
			printVerticalStreamLine $1;
		else
			printBlank;
		fi
	done;

	if [ $1 -eq $2 ]; then 
		exit 1; 
	fi

	next=$(($1/2))
	printSegment $next $2 $3
}

printSegment 64 1 $((2**($n-1)))
