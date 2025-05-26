import numpy as np
import matplotlib.pyplot as plt

# Load data
data = np.loadtxt('fields_00500.dat')
x = data[:, 0].reshape(64, 64)
y = data[:, 1].reshape(64, 64)
vx = data[:, 2].reshape(64, 64)
vy = data[:, 3].reshape(64, 64)
Bx = data[:, 4].reshape(64, 64)
By = data[:, 5].reshape(64, 64)

# Plot
plt.figure(figsize=(12, 10))
plt.subplot(2, 2, 1)
plt.contourf(x, y, vx, cmap='viridis')
plt.colorbar(label='v_x')
plt.title('Velocity v_x')
plt.xlabel('x')
plt.ylabel('y')

plt.subplot(2, 2, 2)
plt.contourf(x, y, vy, cmap='viridis')
plt.colorbar(label='v_y')
plt.title('Velocity v_y')
plt.xlabel('x')
plt.ylabel('y')

plt.subplot(2, 2, 3)
plt.contourf(x, y, Bx, cmap='plasma')
plt.colorbar(label='B_x')
plt.title('Magnetic Field B_x')
plt.xlabel('x')
plt.ylabel('y')

plt.subplot(2, 2, 4)
plt.contourf(x, y, By, cmap='plasma')
plt.colorbar(label='B_y')
plt.title('Magnetic Field B_y')
plt.xlabel('x')
plt.ylabel('y')

plt.tight_layout()
plt.savefig('mhd_fields.png')
plt.show()
