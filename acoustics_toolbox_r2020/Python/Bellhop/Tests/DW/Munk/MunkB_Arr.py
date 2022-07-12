#==================================================================
#  
#  Bellhop - MunkB_Arr: arrival calculation along range / single depth (start at r = 0)
#  Faro, Dom Dez 18 20:57:38 WET 2016
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

print("Bellhop - Munk profile:")
print("Arrivals calculation, ASCII  file output")
print("Geometric hat beams in Cartesian coordinates") 

system("bellhop.exe MunkB_Arr")

Arr, Pos = read_arrivals_asc( 'MunkB_Arr.arr', 18 )

Narr   = int_( squeeze( Arr['Narr'] ) )
delay  = real( squeeze( Arr['delay'] ) )
A      = abs( squeeze( Arr['A'] ) )
rarray = squeeze( Pos['r_range'] )

Nrr = rarray.size

fig = figure()
ax = fig.gca(projection='3d')

for i in range(1,Nrr):
    tau = delay[i,0:Narr[i]]
    amp = abs( A[i,0:Narr[i]] )
    rangei = rarray[i]*ones(Narr[i])
    for j in range(Narr[i]):
        ax.plot([tau[j],tau[j]], [rangei[j],rangei[j]], [0.0,amp[j]],'b')    
        ax.plot([tau[j],tau[j]], [rangei[j],rangei[j]], [0.0,amp[j]],'bo')
xlabel('Travel time (s)')
ylabel('Range (in m)')
title('Bellhop - Munk profile')
show()

print('done.')
