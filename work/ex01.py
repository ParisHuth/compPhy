# Paris J. Huth: Gruppe 1
# Q inich Pakal Figueroa Coc: Gruppe 5

# imports
import numpy as np
import matplotlib.pyplot as plt

# Numerical Integration

def inte(x,a,n):
    return x**n/(x+a);
    
a = 5
x = np.linspace(0,1,100)
n = np.array([1,5,10,20,30,50])

y = np.array([inte(x,a,i) for i in n])

colors = plt.cm.rainbow(np.linspace(0,1,len(n)))
for i in range(len(n)):
    plt.plot(x,y[i],color=colors[i],label='n='+str(n[i]))
    
plt.legend(loc='upper left')
plt.show()    

# b 

def iter(a, n0, y0, n1):
    # strictly going from m->n doesnt need a case by case
    m = min(n0,n1)
    n = max(n0,n1)

    # error handling
    # check for trivial case
    if n == m:
        return y0
    
    if m == 0:
        print('A boundary was set to 0! This would have resulted in an infite', ' result. Replacing boundary with 1.')
        m = 1
    
    # iterating
    y = y0
    for i in range(m,n):
        y = 1/i - a*y
    
    # returning finished result
    return y
   
## c
# running iter function
iter(a=5, n0=0, y0=np.log((1+5)/5), n1=30)
iter(a=5, n0=50, y0=np.log((1+5)/5), n1=30)


