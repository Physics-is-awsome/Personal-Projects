
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import glob
import os

# Parameters
nx, ny = 64, 64  # Grid size
Lx, Ly = 2 * np.pi, 2 * np.pi  # Domain size
dt = 0.005  # Time step from Fortran code
output_interval = 50  # Steps between outputs

# Find all field files
files = sorted(glob.glob('fields_*.dat'), key=lambda x: int(x.split('_')[1].split('.')[0]))

# Read one file to set up grid
data = np.loadtxt(files[0])
x = data[:, 0].reshape(nx, ny)
y = data[:, 1].reshape(nx, ny)

# Set up figure with 2x2 subplots
fig, axes = plt.subplots(2, 2, figsize=(12, 10))
axes = axes.ravel()
titles = ['Velocity v_x', 'Velocity v_y', 'Magnetic Field B_x', 'Magnetic Field B_y']
cmaps = ['viridis', 'viridis', 'plasma', 'plasma']

# Initialize contour plots
contours = []
for i, ax in enumerate(axes):
    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.set_title(titles[i])
    # Initialize with first frame
    field = data[:, i+2].reshape(nx, ny)
    cont = ax.contourf(x, y, field, cmap=cmaps[i], levels=20)
    contours.append(cont)
    fig.colorbar(cont, ax=ax)

# Animation update function
def update(frame):
    # Read data for current frame
    data = np.loadtxt(files[frame])
    vx = data[:, 2].reshape(nx, ny)
    vy = data[:, 3].reshape(nx, ny)
    Bx = data[:, 4].reshape(nx, ny)
    By = data[:, 5].reshape(nx, ny)
    
    # Update each contour plot
    for i, cont in enumerate(contours):
        for c in cont.collections:
            c.remove()  # Clear previous contours
        field = [vx, vy, Bx, By][i]
        contours[i] = axes[i].contourf(x, y, field, cmap=cmaps[i], levels=20)
    
    # Update title with time
    fig.suptitle(f'Time = {frame * output_interval * dt:.2f}')
    return [c for cont in contours for c in cont.collections]

# Create animation
ani = FuncAnimation(fig, update, frames=len(files), interval=200, blit=True)

# Save animation
ani.save('mhd_animation.mp4', writer='ffmpeg', fps=10)
plt.close()

print("Animation saved as mhd_animation.mp4")
