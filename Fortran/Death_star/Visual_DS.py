import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import matplotlib.cm as cm
from matplotlib.colors import LogNorm

# Parameters from Fortran code
nr = 200  # Updated radial points
nt = 100  # Updated angular points
r_planet = 6.371e6  # Planet radius (meters)
dt = 1.0e-3  # Nominal time step (for reference, actual dt varies)
t_max = 10.0  # Simulation time (s)
output_freq = 10  # Output every 10 steps

# Estimate number of steps (adjust based on actual files)
# Check number of files with: ls /home/ajc/Personal-Projects/Fortran/Death_star/output_t*.dat | wc -l
n_steps = 100  # Temporary value for testing; replace with actual number of files

# File path
file_path = '/home/ajc/Personal-Projects/Fortran/Death_star/output_t{0:04d}.dat'

# Set up figure and axis
fig, ax = plt.subplots(figsize=(10, 10), dpi=100)
ax.set_aspect('equal')
ax.set_xlabel('X (Planet Radii)', fontsize=12)
ax.set_ylabel('Y (Planet Radii)', fontsize=12)
ax.set_facecolor('black')

# Add planet outline
circle = plt.Circle((0, 0), 1, color='white', fill=False, linestyle='--', linewidth=2)
ax.add_patch(circle)

# Load initial data
try:
    data = np.loadtxt(file_path.format(1))
except FileNotFoundError:
    print(f"Error: {file_path.format(1)} not found. Check file path or run Fortran code.")
    exit(1)
if data.shape[0] != nr * nt:
    print(f"Error: Expected {nr * nt} rows, got {data.shape[0]}. Check grid size.")
    exit(1)

r = data[:, 0].reshape(nr, nt)
theta = data[:, 1].reshape(nr, nt)
X = r * np.sin(theta) / r_planet
Y = r * np.cos(theta) / r_planet
rho = data[:, 2].reshape(nr, nt)

# Set up colormap with logarithmic scaling
vmin, vmax = max(np.min(rho[rho > 0]), 1.0e-3), np.max(rho)  # Avoid log(0), match rho_min
norm = LogNorm(vmin=vmin, vmax=vmax)
mesh = ax.pcolormesh(X, Y, rho, cmap='inferno', norm=norm, shading='auto')
cbar = fig.colorbar(mesh, ax=ax, label='Density (kg/m³)', pad=0.02)
cbar.ax.tick_params(labelsize=10)

# Add beam impact point
ax.scatter([0], [1], color='cyan', s=100, marker='*', label='Beam Impact')
ax.legend(loc='upper right', fontsize=10)

# Animation update function
def update(frame):
    try:
        data = np.loadtxt(file_path.format(frame + 1))
    except FileNotFoundError:
        print(f"Error: {file_path.format(frame + 1)} not found. Stopping animation.")
        return [mesh]
    rho = data[:, 2].reshape(nr, nt)
    mesh.set_array(rho.ravel())
    # Approximate time (actual dt varies, use nominal dt for display)
    ax.set_title(f'Plasma Beam Impact at t ≈ {(frame * output_freq * dt):.3f} s', fontsize=14)
    return [mesh]

# Create animation
ani = FuncAnimation(fig, update, frames=n_steps, interval=200, blit=True)

# Display animation
plt.show()

# Close plot
plt.close()
