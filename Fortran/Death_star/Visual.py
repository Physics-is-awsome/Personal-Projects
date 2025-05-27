import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

# Load the data
n = 100  # Number of particles (as defined in Fortran)
data = np.loadtxt('particle_positions.dat')
fig, ax = plt.subplots()
ax.set_xlim(-6, 6)
ax.set_ylim(-6, 6)
points, = ax.plot([], [], 'o')

def init():
    points.set_data([], [])
    return points,

def animate(i):
    x = data[i, 1:1+2*n:2]  # Extract x coordinates
    y = data[i, 2:2+2*n:2]  # Extract y coordinates
    points.set_data(x, y)
    ax.set_title(f'Time: {data[i, 0]:.2f}, Particles: {n}')
    return points,

ani = animation.FuncAnimation(fig, animate, init_func=init, frames=data.shape[0], interval=50, blit=True)
plt.show()
