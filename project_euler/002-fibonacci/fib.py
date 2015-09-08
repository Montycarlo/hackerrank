
t = int(raw_input()) 

xs = [1,2]
def gennext(): xs.append(xs[-1] + xs[-2])

class Fibgen:
  def __init__(self, max): 
    self.i = None
    self.max = max
  def __iter__(self):
    self.i = -1;
    return self
  def next(self):
    self.i += 1
    while len(xs) < self.i+1: 
      for z in range(5): 
        gennext()
    if xs[self.i] > self.max: raise StopIteration
    return xs[self.i]


for num in range(t):
  n = int(raw_input())
  sum = 0
  for x in Fibgen(n): 
    if x % 2 != 0: continue
    sum += x
  print sum
      

