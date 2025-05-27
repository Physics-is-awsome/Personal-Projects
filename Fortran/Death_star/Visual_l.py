import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

n = 100
data = np.loadtxt('particle_positions.dat')
fig, ax = plt.subplots()
ax.set_xlim(-6, 6)
ax.set_ylim(-6, 6)
scatter = ax.scatter([], [], c=[], s=10, cmap='viridis')
trails = [ax.plot([], [], 'k-', alpha=0.2, linewidth=0.5)[0] for _ in range(n)]
trail_length = 10
past_positions = [[] for _ in range(n)]
laser_time = 5.0  # From Fortran
laser_width = 0.1

def init():
    scatter.set_offsets(np.empty((0, 2)))
    scatter.set_array(np.array([]))
    for trail in trails:
        trail.set_data([], [])
    return [scatter] + trails

def animate(i):
    x = data[i, 1:1+3*n:3]
    y = data[i, 2:2+3*n:3]
    rho = data[i, 3:3+3*n:3]
    positions = np.vstack((x, y)).T
    colors = rho  # Default: color by density
    # Highlight laser-affected particles in red
    if abs(data[i, 0] - laser_time) < dt:  # dt from Fortran
        colors = np.where((x > 0) & (np.abs(y) < laser_width), -1, rho)  # -1 for red
        scatter.set_cmap('viridis')
        scatter.set_clim(vmin=np.min(rho), vmax=np.max(rho))
    scatter.set_offsets(positions)
    scatter.set_array(colors)
    for j in range(n):
        past_positions[j].append([x[j], y[j]])
        if len(past_positions[j]) > trail_length:
            past_positions[j].pop(0)
        trail_x, trail_y = zip(*past_positions[j]) if past_positions[j] else ([], [])
        trails[j].set_data(trail_x, trail_y)
    ax.set_title(f'Time: {data[i, 0]:.2f}, Particles: {n}')
    return [scatter] + trails

ani = animation.FuncAnimation(fig, animate, init_func=init, frames=data.shape[0], interval=50, blit=True)
plt.colorbar(scatter, label='Density (red = laser impact)')
plt.show()
