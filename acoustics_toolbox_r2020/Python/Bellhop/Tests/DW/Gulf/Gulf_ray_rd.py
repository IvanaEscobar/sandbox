#=======================================================================
# 
# Bellhop: Ellipsoidal boundaries
# Mexilhoeira Grande, Dom Jul 30 14:21:39 WEST 2017
# Written by Tordar 
# 
#=======================================================================

from os import system
from numpy import *
from scipy.io import *
from pylab import *
sys.path.append ("/home/orodrig/FORdoc/at/Python/")
from plotray import *

print("Bellhop - Topgulf, Range dep.:")
print("Ray trace run")
print("Geometric hat beams in Cartesian coordinates") 

fid = open('Gulf_ray_rd.bty','r')
theline = fid.readline()
theline = fid.readline()
n       = int( theline )
rbtykm  = zeros( n ) 
zbty    = zeros( n )
for i in range(n):
    theline = str( fid.readline() )
    datai = theline.split()
    rbtykm[ i] = float( datai[0] )
    zbty[   i] = float( datai[1] )
fid.close()

rbty = rbtykm*1000

system("bellhop.exe Gulf_ray_rd")

figure(1)
plotray('Gulf_ray_rd.ray')
plot(rbty,-zbty,'k',linewidth=2)
xlabel('Range (m)')
ylabel('Depth (m)')
title("Bellhop - Topgulf, Range dep.")

show()

print("done.")

