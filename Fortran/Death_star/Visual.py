import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

data = np.loadtxt('particle_positions.dat')
n = (data.shape[1] - 1) // 2  # Number of particles
fig, ax = plt.subplots()
ax.set_xlim(-6, 6)
ax.set_ylim(-6, 6)
points, = ax.plot([], [], 'o')

def init():
    points.set_data([], [])
    return points,

def animate(i):
    x = data[i, 1::2]
    y = data[i, 2::2]
    points.set_data(x, y)
    return points,

ani = animation.FuncAnimation(fig, animate, init_func=init, frames=data.shape[0], interval=50, blit=True)
plt.show()
