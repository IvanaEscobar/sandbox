#==================================================================
#  
#  Bellhop: Block case, geometric beams
#  Mexilhoeira Grande, Dom Jul 30 11:58:40 WEST 2017
#  Written by Tordar
#  
#==================================================================

# ipython: run blockB_geo

from os import system
from numpy import *
from scipy.io import *
from pylab import *
sys.path.append ("/home/orodrig/FORdoc/at/Python/")
from readshd import *

rs = 0.0

print("Bellhop - Block case, geometric beams:")
print("Coherent TL calculation")
print("Geometric hat beams in Cartesian coordinates")

fid = open('blockB_geo.bty','r')
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

system("bellhop.exe blockB_geo")

filename = 'blockB_geo.shd'
xs = nan
ys = nan
pressure,geometry = readshd(filename,xs,ys)

zs     = geometry["zs"]
rarray = geometry["rarray"]; rarraykm = rarray/1000
zarray = geometry["zarray"]

Dmax = zarray[-1]
rmax = rarray[-1]; rmaxkm = rmax/1000

p = squeeze( pressure, axis=(0,1) )
tl = -20*log10( abs( p ) )
tl[ tl > 160.0 ] = 160.0

figure(1)
imshow(tl,extent=[-rmaxkm,rmaxkm,0,Dmax],aspect='auto',cmap='jet_r',origin='lower')
plot(rbtykm,zbty,'k')
cb = colorbar()
cb.ax.invert_yaxis()
plot(rs,zs,marker="<",markersize=16,color="k")
xlabel('Range (km)')
ylabel('Depth (m)')
title('Bellhop - Block case, geometric beams')
xlim(0,rmaxkm)
ylim(Dmax,0)

show()

print("done.")
