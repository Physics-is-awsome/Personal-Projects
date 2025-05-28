import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from matplotlib.colors import LogNorm
import os

# Parameters from Fortran code
nr = 200
nt = 100
r_planet = 6.371e6
output_freq = 10

# Count output files
file_path_template = '/home/ajc/Personal-Projects/Fortran/Death_star/output_t{0:04d}.dat'
output_files = [f for f in os.listdir('/home/ajc/Personal-Projects/Fortran/Death_star/') 
                if f.startswith('output_t') and f.endswith('.dat')]
n_steps = len(output_files)
print(f"Found {n_steps} output files")

# Set up figure
fig, ax = plt.subplots(figsize=(10, 10), dpi=100)
ax.set_aspect('equal')
ax.set_xlabel('X (Planet Radii)', fontsize=12)
ax.set_ylabel('Y (Planet Radii)', fontsize=12)
ax.set_facecolor('black')
circle = plt.Circle((0, 0), 1, color='white', fill=False, linestyle='--', linewidth=2)
ax.add_patch(circle)

# Load initial data
try:
    data = np.loadtxt(file_path_template.format(1), skiprows=1)
    t_initial = np.loadtxt(file_path_template.format(1), max_rows=1)
except FileNotFoundError:
    print(f"Error: {file_path_template.format(1)} not found.")
    exit(1)
if data.shape[0] != nr * nt:
    print(f"Error: Expected {nr * nt} rows, got {data.shape[0]}.")
    exit(1)

r = data[:, 0].reshape(nr, nt)
theta = data[:, 1].reshape(nr, nt)
rho = data[:, 2].reshape(nr, nt)

# Mirror for full circle
nt_full = 2 * nt - 1
theta_full = np.linspace(0, 2 * np.pi, nt_full)
X_full = np.zeros((nr, nt_full))
Y_full = np.zeros((nr, nt_full))
rho_full = np.zeros((nr, nt_full))
X_full[:, :nt] = r * np.sin(theta) / r_planet
Y_full[:, :nt] = r * np.cos(theta) / r_planet
rho_full[:, :nt] = rho
theta_mirror = np.linspace(np.pi, 2 * np.pi, nt)[:, None]
r_mirror = r[:, :nt]
X_full[:, nt-1:] = r_mirror * np.sin(theta_mirror.T) / r_planet
Y_full[:, nt-1:] = r_mirror * np.cos(theta_mirror.T) / r_planet
rho_full[:, nt-1:] = rho[:, ::-1]

# Colormap
vmin = max(np.min(rho_full[rho_full > 0]), 1.0e-3)  # Match rho_min
vmax = max(np.max(rho_full) * 1.1, 5000.0)  ! Adjusted for granite-like rho
norm = LogNorm(vmin=vmin, vmax=vmax)
mesh = ax.pcolormesh(X_full, Y_full, rho_full, cmap='inferno', norm=norm, shading='auto')
cbar = fig.colorbar(mesh, ax=ax, label='Density (kg/m³)', pad=0.02)
cbar.ax.tick_params(labelsize=10)
ax.scatter([0], [1], color='cyan', s=100, marker='*', label='Beam Impact')
ax.legend(loc='upper right', fontsize=10)

# Animation update
def update(frame):
    try:
        data = np.loadtxt(file_path_template.format(frame + 1), skiprows=1)
        t_actual = np.loadtxt(file_path_template.format(frame + 1), max_rows=1)
    except FileNotFoundError:
        print(f"Error: {file_path_template.format(frame + 1)} not found.")
        return [mesh]
    rho = data[:, 2].reshape(nr, nt)
    rho_full[:, :nt] = rho
    rho_full[:, nt-1:] = rho[:, ::-1]
    mesh.set_array(rho_full.ravel())
    ax.set_title(f'Plasma Beam Impact at t = {t_actual:.3f} s', fontsize=14)
    return [mesh]

# Create animation
ani = FuncAnimation(fig, update, frames=n_steps, interval=200, blit=True)
plt.show()
plt.close()
