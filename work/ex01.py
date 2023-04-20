# Paris J. Huth: Gruppe 1
# Q inich Pakal Figueroa Coc: Gruppe 5

# imports
import numpy as np
import matplotlib.pyplot as plt

# Numerical Integration

def inte(x,a,n):
    return x**n/(x+a)
    
a = 5
x = np.linspace(0,1,100)
n = np.array([1,5,10,20,30,50])

y = np.array([inte(x,a,i) for i in n])

for i in range(length(n)):
    plt.plot(x,y[i])
    
plt.show()    
