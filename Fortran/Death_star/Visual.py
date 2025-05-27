import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

data = np.loadtxt('particle_positions.dat')
n = (data.shape[1] - 2) // 3  # Number of particles
fig, ax = plt.subplots()
ax.set_xlim(-6, 6)
ax.set_ylim(-6, 6)
points, = ax.plot([], [], 'o')

def init():
    points.set_data([], [])
    return points,

def animate(i):
    n_active = int(data[i, 1])
    active = data[i, 2+2*n:2+3*n].astype(bool)
    x = data[i, 2:2+2*n:2][active]
    y = data[i, 3:3+2*n:2][active]
    points.set_data(x, y)
    ax.set_title(f'Time: {data[i, 0]:.2f}, Active Particles: {n_active}')
    return points,

ani = animation.FuncAnimation(fig, animate, init_func=init, frames=data.shape[0], interval=50, blit=True)
plt.show()
