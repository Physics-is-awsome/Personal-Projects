import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from matplotlib.colors import LogNorm
import os

# Parameters from Fortran code
nr = 200  # Radial points
nt = 100  # Angular points
r_planet = 6.371e6  # Planet radius (meters)
dt = 1.0e-3  # Nominal time step (for display, actual dt varies)
t_max = 10.0  # Simulation time (s)
output_freq = 10  # Output every 10 steps

# Count actual output files
file_path_template = '/home/ajc/Personal-Projects/Fortran/Death_star/output_t{0:04d}.dat'
output_files = [f for f in os.listdir('/home/ajc/Personal-Projects/Fortran/Death_star/') 
                if f.startswith('output_t') and f.endswith('.dat')]
n_steps = len(output_files)  # Should be 1000
print(f"Found {n_steps} output files")

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
    data = np.loadtxt(file_path_template.format(1))
except FileNotFoundError:
    print(f"Error: {file_path_template.format(1)} not found. Check file path.")
    exit(1)
if data.shape[0] != nr * nt:
    print(f"Error: Expected {nr * nt} rows, got {data.shape[0]}. Check grid size.")
    exit(1)

r = data[:, 0].reshape(nr, nt)
theta = data[:, 1].reshape(nr, nt)
rho = data[:, 2].reshape(nr, nt)

# Mirror data for full circle (theta from 0 to 2*pi)
nt_full = 2 * nt - 1  # 2 * 100 - 1 = 199 points
theta_full = np.linspace(0, 2 * np.pi, nt_full)
# Create full arrays
X_full = np.zeros((nr, nt_full))
Y_full = np.zeros((nr, nt_full))
rho_full = np.zeros((nr, nt_full))

# Fill first half (0 to pi)
X_full[:, :nt] = r * np.sin(theta) / r_planet
Y_full[:, :nt] = r * np.cos(theta) / r_planet
rho_full[:, :nt] = rho

# Fill second half (pi to 2*pi) using symmetry
theta_mirror = 2 * np.pi - theta[:, ::-1][:, 1:]  # Reverse theta, skip theta=pi
X_full[:, nt-1:] = r[:, :nt-1] * np.sin(theta_mirror) / r_planet
Y_full[:, nt-1:] = r[:, :nt-1] * np.cos(theta_mirror) / r_planet
rho_full[:, nt-1:] = rho[:, ::-1][:, 1:]  # Mirror density

# Set up colormap with fixed scaling
vmin = max(np.min(rho_full[rho_full > 0]), 1.0e-3)  # Match Fortran rho_min
vmax = np.max(rho_full) * 1.1
norm = LogNorm(vmin=vmin, vmax=vmax)
mesh = ax.pcolormesh(X_full, Y_full, rho_full, cmap='inferno', norm=norm, shading='auto')
cbar = fig.colorbar(mesh, ax=ax, label='Density (kg/m³)', pad=0.02)
cbar.ax.tick_params(labelsize=10)

# Add beam impact point
ax.scatter([0], [1], color='cyan', s=100, marker='*', label='Beam Impact')
ax.legend(loc='upper right', fontsize=10)

# Animation update function
def update(frame):
    try:
        data = np.loadtxt(file_path_template.format(frame + 1))
    except FileNotFoundError:
        print(f"Error: {file_path_template.format(frame + 1)} not found.")
        return [mesh]
    rho = data[:, 2].reshape(nr, nt)
    # Mirror data
    rho_full[:, :nt] = rho
    rho_full[:, nt-1:] = rho[:, ::-1][:, 1:]
    mesh.set_array(rho_full.ravel())
    ax.set_title(f'Plasma Beam Impact at t ≈ {(frame * output_freq * dt):.3f} s', fontsize=14)
    return [mesh]

# Create animation
ani = FuncAnimation(fig, update, frames=n_steps, interval=200, blit=True)

# Display animation
plt.show()

# Close plot
plt.close()
