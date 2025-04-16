import numpy as np
import matplotlib.pyplot as plt
from scipy import stats

#Mean
i=0
mu=0
x=0
y=0
while i>=0 and i<10:
    mu=mu+y
    i+=1
    x+=x
    y=np.cos(x)**2+np.log(x+2)-np.sin(3*x)
else:
    mu=mu/i
print(mu)
#Defining equation
def my_equation(x):
    return np.cos(x)**2 + np.log(x+2) - np.sin(3*x)
x_value=np.linspace(0, 10, 1000)

y_value=my_equation(x_value)
#Defining mode
mode=stats.mode(y_value)
print("Mode: ", mode)

#Median
median=np.median(y_value)
print("Median: ", median)

#Variance
variance=np.var(y_value)
print("Variance: ", variance)

#Standard Deviation
std_dev=np.std(y_value)
print("Standard Deviation: ", std_dev)

#Plotting the graph
plt.plot(x_value, y_value)
plt.title("SVA 16")
plt.xlabel("X-axis")
plt.ylabel("Y-axis")
plt.legend(["y=cos(x)^2 + log(x+2) - sin(3x)"])
plt.grid()
plt.show()
