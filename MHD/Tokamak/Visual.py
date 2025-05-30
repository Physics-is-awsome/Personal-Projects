import h5py
import numpy as np
import matplotlib
matplotlib.use('Agg')  # Non-GUI backend for saving
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# Grid
R = np.linspace(4.0, 8.0, 50)
Z = np.linspace(-2.0, 2.0, 50)
R, Z = np.meshgrid(R, Z)

# Animation setup
fig, ax = plt.subplots(figsize=(8, 6))

def update(n):
    ax.clear()
    with h5py.File(f'output_{n*100}.h5', 'r') as f:
        Bmag = np.array(f['B_magnitude'])
    im = ax.contourf(R, Z, Bmag, cmap='cool', levels=20)
    ax.set_title(f'Magnetic Field (T) at Step {n*100}')
    ax.set_xlabel('R (m)')
    ax.set_ylabel('Z (m)')
    ax.set_aspect('equal')
    return im,

# Create animation (steps 100, 200, ..., 1000)
ani = FuncAnimation(fig, update, frames=range(1, 11), blit=False)
plt.colorbar(ax.contourf(R, Z, np.zeros_like(R), cmap='cool'), ax=ax)

# Save animation
ani.save('Bmag_animation.mp4', writer='ffmpeg', fps=5)

# Save final plots
with h5py.File('output_final.h5', 'r') as f:
    v = np.array(f['velocity_v'])
    T = np.array(f['temperature'])
    rho = np.array(f['density'])
    Bmag = np.array(f['B_magnitude'])

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
plt.savefig('tokamak_final_plots.png')
