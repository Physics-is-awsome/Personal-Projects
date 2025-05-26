import matplotlib
matplotlib.use('Agg')
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import glob
import os

# Parameters
nx, ny, nz = 32, 32, 32
Lx, Ly, Lz = 0.1, 0.02, 0.02  # Channel dimensions
dt = 0.002
output_interval = 30  # Match Fortran output
slice_y = ny // 2  # Slice at y = Ly/2

# Find files
files = sorted(glob.glob('fields_*.dat'), key=lambda x: int(x.split('_')[1].split('.')[0]))
if not files:
    raise FileNotFoundError("No fields_*.dat files found.")

# Read grid
data = np.loadtxt(files[0], comments='#')
x = data[:, 0].reshape(nx, ny, nz)[:, slice_y, :]
z = data[:, 2].reshape(nx, ny, nz)[:, slice_y, :]

# Setup figure
fig, ax = plt.subplots(figsize=(8, 4))
ax.set_xlabel('x (m)')
ax.set_ylabel('z (m)')
ax.set_title('Velocity v_x in MHD Thruster')
field = data[:, 3].reshape(nx, ny, nz)[:, slice_y, :]  # v_x
im = ax.imshow(field, extent=[0, Lx, 0, Lz], cmap='viridis', origin='lower', vmin=-0.1, vmax=0.1)
fig.colorbar(im, ax=ax, label='v_x (m/s)')

# Animation update
def update(frame):
    data = np.loadtxt(files[frame], comments='#')
    field = data[:, 3].reshape(nx, ny, nz)[:, slice_y, :]  # v_x
    im.set_array(field)
    fig.suptitle(f'Time = {frame * output_interval * dt:.3f} s')
    return [im]

# Create animation
ani = FuncAnimation(fig, update, frames=len(files), interval=200, blit=True)

# Save
save_path = '/mnt/c/Users/<YourUsername>/Desktop/mhd_thruster.mp4'  # Adjust path
try:
    ani.save(save_path, writer='ffmpeg', fps=5, dpi=100)
    print(f"Animation saved to: {save_path}")
except Exception as e:
    print(f"Error saving animation: {e}")
    print("Ensure ffmpeg is installed: sudo apt-get install ffmpeg")
plt.close(fig)
