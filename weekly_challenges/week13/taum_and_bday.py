# Python 3
# https://www.hackerrank.com/contests/w13/challenges/taum-and-bday

T = int(input(''))
for i in range(T):
    b_w = input('').split(' ') 
    count_b = int(b_w[0])
    count_w = int(b_w[1])
    x_y_z = input('').split(' ')
    x = int(x_y_z[0])
    y = int(x_y_z[1])
    z = int(x_y_z[2])
    cost = count_b*x + count_w*y
    if z < y or y < x:
        if x > y+z:
            cost = count_b*(y+z) + count_w*y
        elif x+z < y:
            cost = count_b*x + count_w*(x+z)
    print(cost)

