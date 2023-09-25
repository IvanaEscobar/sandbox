import numpy as np

import matplotlib.plt as plt
import cmocean.cm as cm

with open('diags/ssp.0000000009.data', 'rb') as f:
    array = np.fromfile(f, dtype='>f')

arr = np.reshape(array, (62,62,15), order='F')
arr = arr[1:61, 1:61, :]

# Plot
fig=plt.figure(figsize=(12,10), dpi=180 )
plt.pcolormesh(arr[:,:,0], cmap=cm.speed, \
        vmin=arr[:,:,0].min(), vmax=arr[:,:,0].max() )
plt.colorbar()
plt.savefig('/home/ivana/regionalgcm/img/bc_gyre_ssss_z1_t9_MPI.png',\
        bbox_inches='tight',transparent=True)
