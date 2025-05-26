import numpy as np
data = np.loadtxt('energy.dat') 
plt.plot(data[:,0], data[:,1])
plt.xlabel('Time (s)')
plt.ylabel('Energy (J)')
plt.savefig('energy.png')
