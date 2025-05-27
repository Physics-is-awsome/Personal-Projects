import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# Parameters
nr = 100
nt = 50
r_planet = 6.371e6
n_steps = 10  # Number of time steps (adjust based on your files)

# Set up figure
fig, ax = plt.subplots(figsize=(8, 8))
ax.set_aspect('equal')
ax.set_xlabel('X (Planet Radii)')
ax.set_ylabel('Y (Planet Radii)')
circle = plt.Circle((0, 0), 1, color='black', fill=False, linestyle='--')
ax.add_patch(circle)

# Load initial data for setting up plot
data = np.loadtxt('  output_t0001.dat')
r = data[:, 0].reshape(nr, nt)
theta = data[:, 1].reshape(nr, nt)
X = r * np.sin(theta) / r_planet
Y = r * np.cos(theta) / r_planet
rho = data[:, 2].reshape(nr, nt)

# Initial plot
mesh = ax.pcolormesh(X, Y, rho, cmap='viridis', shading='auto')
cbar = fig.colorbar(mesh, ax=ax, label='Density (kg/m³)')

# Animation update function
def update(frame):
    data = np.loadtxt(f'output_t{frame+1}.dat')
    rho = data[:, 2].reshape(nr, nt)
    mesh.set_array(rho.ravel())
    ax.set_title(f'Density at Time Step {frame+1}')
    return [mesh]

# Create animation
ani = FuncAnimation(fig, update, frames=n_steps, interval=200, blit=True)

# Save or show animation
ani.save('planet_destruction.mp4', writer='ffmpeg')
# plt.show()  # Uncomment to display instead of saving

plt.close()
