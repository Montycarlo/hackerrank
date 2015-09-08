T = int(input(''))
for t in range(T):
    upper = int(input(''))
    nos = []
    i=1
    if (len(nos)>0): 
        i = nos[-1]
    if i > upper: 
    i_sum = 0
    while i<upper:
        if i%3 == 0 or i%5 == 0:
            nos.append(i)
            i_sum += i

    print(i_sum)
