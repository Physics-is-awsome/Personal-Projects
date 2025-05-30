import h5py
import numpy as np
import matplotlib.pyplot as plt

# Read HDF5 file
with h5py.File('output.h5', 'r') as f:
    v = np.array(f['velocity_v'])
    T = np.array(f['temperature'])
    rho = np.array(f['density'])
    Bmag = np.array(f['B_magnitude'])

# Grid
R = np.linspace(4.0, 8.0, 50)
Z = np.linspace(-2.0, 2.0, 50)
R, Z = np.meshgrid(R, Z)

# Plot
fig, axs = plt.subplots(2, 2, figsize=(12, 10))

im0 = axs[0, 0].contourf(R, Z, v, cmap='viridis')
axs[0, 0].set_title('Velocity (v)')
axs[0, 0].set_xlabel('R (m)')
axs[0, 0].set_ylabel('Z (m)')
fig.colorbar(im0, ax=axs[0, 0])

im1 = axs[0, 1].contourf(R, Z, T, cmap='hot')
axs[0, 1].set_title('Temperature (K)')
axs[0, 1].set_xlabel('R (m)')
axs[0, 1].set_ylabel('Z (m)')
fig.colorbar(im1, ax=axs[0, 1])

im2 = axs[1, 0].contourf(R, Z, rho, cmap='plasma')
axs[1, 0].set_title('Density (kg/m^3)')
axs[1, 0].set_xlabel('R (m)')
axs[1, 0].set_ylabel('Z (m)')
fig.colorbar(im2, ax=axs[1, 0])

im3 = axs[1, 1].contourf(R, Z, Bmag, cmap='cool')
axs[1, 1].set_title('Magnetic Field (T)')
axs[1, 1].set_xlabel('R (m)')
axs[1, 1].set_ylabel('Z (m)')
fig.colorbar(im3, ax=axs[1, 1])

plt.tight_layout()
plt.show()
