#==================================================================
#  
#  Bellhop: Arctic sound speed profile
#  Faro, Ter Jul  4 22:50:28 WEST 2017
#  Written by Tordar
#  
#==================================================================

# ipython: run showssp

from os import *
import sys
from numpy import *
from scipy.io import *
from pylab import *

sspdata = loadtxt("sound_speed.dat")

z = sspdata[:,0]; Dmax = max( z )
c = sspdata[:,1]

figure(1)
plot(c,-z)
xlabel("Sound Speed (m/s)")
ylabel("Depth (m)")
title("Bellhop - Arctic SSP")
grid(True)
show()

print("done.")
