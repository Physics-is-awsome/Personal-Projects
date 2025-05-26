import matplotlib
matplotlib.use('Agg')  # Non-interactive backend
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import glob
import os

# Parameters
nx, ny, nz = 32, 32, 32  # Grid size
Lx, Ly, Lz = 2 * np.pi, 2 * np.pi, 2 * np.pi  # Domain size
dt = 0.002  # Time step from Fortran code
output_interval = 100  # Steps between outputs
slice_z = nz // 2  # Slice at z ≈ π (index 16)

# Find all field files
files = sorted(glob.glob('fields_*.dat'), key=lambda x: int(x.split('_')[1].split('.')[0]))
if not files:
    raise FileNotFoundError("No fields_*.dat files found in the current directory.")

# Read one file to set up grid
try:
    data = np.loadtxt(files[0], comments='#')
except Exception as e:
    raise IOError(f"Error reading {files[0]}: {e}")
if data.shape[1] != 9:
    raise ValueError("Unexpected data format in fields_*.dat files.")
x = data[:, 0].reshape(nx, ny, nz)[:, :, slice_z]
y = data[:, 1].reshape(nx, ny, nz)[:, :, slice_z]

# Set up figure with 2x3 subplots for vx, vy, vz, Bx, By, Bz
fig, axes = plt.subplots(2, 3, figsize=(15, 10))
axes = axes.ravel()
titles = ['Velocity v_x', 'Velocity v_y', 'Velocity v_z', 'Magnetic Field B_x', 'Magnetic Field B_y', 'Magnetic Field B_z']
cmaps = ['viridis', 'viridis', 'viridis', 'plasma', 'plasma', 'plasma']

# Initialize plots
ims = []
for i, ax in enumerate(axes):
    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.set_title(titles[i])
    field = data[:, i+3].reshape(nx, ny, nz)[:, :, slice_z]
    im = ax.imshow(field, extent=[0, Lx, 0, Ly], cmap=cmaps[i], origin='lower', vmin=-0.2, vmax=0.2)
    ims.append(im)
    fig.colorbar(im, ax=ax)

# Animation update function
def update(frame):
    try:
        data = np.loadtxt(files[frame], comments='#')
        for i in range(6):
            field = data[:, i+3].reshape(nx, ny, nz)[:, :, slice_z]
            ims[i].set_array(field)
        fig.suptitle(f'Time = {frame * output_interval * dt:.2f}')
        return ims
    except Exception as e:
        print(f"Error processing {files[frame]}: {e}")
        return ims

# Create animation
ani = FuncAnimation(fig, update, frames=len(files), interval=200, blit=True)

# Save animation
try:
    ani.save('mhd_3d_slices.mp4', writer='ffmpeg', fps=10, dpi=100)
    print("Animation saved as mhd_3d_slices.mp4")
except Exception as e:
    print(f"Error saving animation: {e}")
    print("Ensure ffmpeg is installed and accessible (run 'ffmpeg -version').")
plt.close(fig)  # Close figure explicitly after saving
