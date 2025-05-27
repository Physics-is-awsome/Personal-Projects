import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

# Load the data, specifying columns to avoid logical values
n = 1000  # Number of particles (as defined in Fortran)
data = np.loadtxt('particle_positions.dat', usecols=range(2*n + 2))  # Load t, n_active, x1, y1, ..., xn, yn
fig, ax = plt.subplots()
ax.set_xlim(-6, 6)
ax.set_ylim(-6, 6)
points, = ax.plot([], [], 'o')

def init():
    points.set_data([], [])
    return points,

def animate(i):
    # Load active flags as strings and convert to boolean
    with open('particle_positions.dat', 'r') as f:
        lines = f.readlines()
        active = np.array(lines[i].split()[2+2*n:2+3*n], dtype=str) == 'T'
    n_active = int(data[i, 1])
    x = data[i, 2:2+2*n:2][active]
    y = data[i, 3:3+2*n:2][active]
    points.set_data(x, y)
    ax.set_title(f'Time: {data[i, 0]:.2f}, Active Particles: {n_active}')
    return points,

ani = animation.FuncAnimation(fig, animate, init_func=init, frames=data.shape[0], interval=50, blit=True)
plt.show()
