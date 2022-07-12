#=======================================================================
# 
# Bellhop: Dickins Seamount
# Mexilhoeira Grande, Sex Jul 28 17:15:48 WEST 2017
# Written by Tordar 
# 
#=======================================================================

from os import system
from numpy import *
from scipy.io import *
from pylab import *
sys.path.append ("/home/orodrig/FORdoc/at/Python/")
from plotray import *

print("Bellhop - Dickins Seamount:") 
print("ray calculation including bathymetry") 

fid = open('DickinsBray.bty','r')
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

system("bellhop.exe DickinsBray")

figure(1)
plotray('DickinsBray.ray')
plot(rbty,-zbty,'k',linewidth=2)
xlabel('Range (m)')
ylabel('Depth (m)')
title("Bellhop - Dickins Seamount")

show()

print("done.")

