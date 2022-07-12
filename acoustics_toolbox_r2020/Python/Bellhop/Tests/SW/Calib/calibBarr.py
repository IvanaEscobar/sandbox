#==================================================================
#  
#  Bellhop - Calibration  case, Gaussian beams
#  AQUA, seg jul 31 17:09:27 WEST 2017
#  Written by Tordar
#  
#==================================================================

from os import *
import sys
sys.path.append ("/home/orodrig/FORdoc/at/Python/")
from numpy import *
from scipy.io import *
from pylab import *
from mpl_toolkits.mplot3d import Axes3D
from read_arrivals_asc import *

print('Bellhop - Calibration case, Gaussian beams') 

system("bellhop.exe calibBarr")

Arr, Pos = read_arrivals_asc( 'calibBarr.arr', 18 )

Narr   = int_( squeeze( Arr['Narr']  ) )
delay  = real( squeeze( Arr['delay'] ) )
A      =  abs( squeeze( Arr['A'] ) )
rarray = squeeze( Pos['r_range'] )
zarray = squeeze( Pos['r_depth'] )

Nrr = rarray.size
Nrd = zarray.size

# Plotting only first hydrophone: 

NARR  = Narr[   :,0]
DELAY = delay[:,:,0]
AMP   = A[    :,:,0]

fig = figure()
ax = fig.gca(projection='3d')

# Plotting only first hydrophone: 

for i in range(1,Nrr):
    tau = DELAY[i,0:NARR[i]]
    amp = abs( AMP[i,0:NARR[i]] )
    rangei = rarray[i]*ones(NARR[i])
    for j in range(NARR[i]):
        ax.plot([tau[j],tau[j]], [rangei[j],rangei[j]], [0.0,amp[j]],'b')    
        ax.plot([tau[j],tau[j]], [rangei[j],rangei[j]], [0.0,amp[j]],'bo')
xlabel('Travel time (s)')
ylabel('Range (in m)')
title('Bellhop - Calibration case, Gaussian beams Hyd1')
show()

print('done.')
