import h5py
import numpy as np
import matplotlib.pyplot as plt

# Open the HDF5 file in read mode
file_path = 'velocity_v.h5'
with h5py.File(file_path, 'r') as file:
    # Access the "velocity_v" dataset
    v_data = np.array(file['velocity_v'])

# Create a heatmap
plt.figure(figsize=(10, 8))
plt.imshow(v_data, cmap='viridis')  # 'viridis' is a good colormap for scientific data
plt.colorbar(label='Velocity v')
plt.title('2D Velocity Field (m/s)')
plt.xlabel('X Index')
plt.ylabel('Y Index')

# Optionally, add grid lines for better readability
plt.grid(True, linestyle='--', alpha=0.7)

# Show the plot
plt.show()

# Save the plot as an image (optional)
plt.savefig('velocity_v_heatmap.png', dpi=300, bbox_inches='tight')
plt.close()
