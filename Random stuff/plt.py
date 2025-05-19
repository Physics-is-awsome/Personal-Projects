import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(0.0,5, 100)
y = -x**2

#r_x = [round(point, 2) for point in x]
#r_y = [round(point, 2) for point in y]
#plt.style.use('seaborn-v0_8-dark')
#coeffs = np.polyfit(x, y, 1)  # Fit a polynomial of degree 1 (linear fit)
#fit_line = np.polyval(coeffs, x) 

#plt.plot(x, fit_line, 'r', label='v1')

plt.plot(x, y)  # Red dots for data points
#plt.grid(True, linestyle='--', alpha=1)
# Add a horizontal line at y = 0 for reference
plt.annotate('', xy=(4.5, -5), xytext=(0, -5), arrowprops=dict(facecolor='black', arrowstyle='->'), fontsize=12)  # x-axis arrow
plt.annotate('', xy=(0, 2), xytext=(0, -5), arrowprops=dict(facecolor='black', arrowstyle='->'), fontsize=12)  # y-axis arrow
plt.xticks(ticks=[])  # Custom x-tick labels
plt.yticks(ticks=[])  # Use exact y-values as y-ticks
#plt.text(3.3, -0.1, 'y', fontsize=12, color='black')  # Label for the x-axis arrow
#plt.text(-0.03, 5.2, 'v', fontsize=12, color='black')  # Label for the y-axis arrow
plt.ylabel('Pressure', fontsize=14)
plt.xlabel('Time', fontsize=14)
plt.tight_layout()
plt.xlim(0, 4.5)  # x-axis from 0 to 18
plt.ylim(-5, 2)   # y-axis from 0 to 8
plt.show()
plt.savefig('APF.png', dpi=300, bbox_inches='tight')

#m, b = coeffs  # Extract slope (m) and intercept (b) from the coefficients
#print(f"slope: {m}, coefficient: {b}")  # Print slope and intercept
