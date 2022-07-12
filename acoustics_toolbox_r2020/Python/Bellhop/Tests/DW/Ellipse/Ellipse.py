#=======================================================================
# 
# Bellhop: Ellipsoidal boundaries
# Mexilhoeira Grande, Dom Jul 30 11:53:49 WEST 2017
# Written by Tordar 
# 
#=======================================================================

from os import system
from numpy import *
from scipy.io import *
from pylab import *
sys.path.append ("/home/orodrig/FORdoc/at/Python/")
from plotray import *

print("Bellhop - Ellipsoidal boundaries:") 
print("ray calculation including bathymetry")

fid = open('Ellipse.ati','r')
theline = fid.readline()
theline = fid.readline()
n       = int( theline )
ratikm  = zeros( n ) 
zati    = zeros( n )
for i in range(n):
    theline = str( fid.readline() )
    datai = theline.split()
    ratikm[ i] = float( datai[0] )
    zati[   i] = float( datai[1] )
fid.close()

rati = ratikm*1000

fid = open('Ellipse.bty','r')
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

system("bellhop.exe Ellipse")

figure(1)
plotray('Ellipse.ray')
plot(rati,-zati,'b',linewidth=2)
plot(rbty,-zbty,'k',linewidth=2)
xlabel('Range (m)')
ylabel('Depth (m)')
title("Bellhop - Ellipsoidal boundaries")

show()

print("done.")

