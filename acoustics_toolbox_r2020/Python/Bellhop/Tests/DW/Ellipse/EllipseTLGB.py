#==================================================================
#  
#  Bellhop: Ellipsoidal boundaries
#  Mexilhoeira Grande, Dom Jul 30 11:58:40 WEST 2017
#  Written by Tordar
#  
#==================================================================

# ipython: run EllipseTLGB

from os import system
from numpy import *
from scipy.io import *
from pylab import *
sys.path.append ("/home/orodrig/FORdoc/at/Python/")
from readshd import *

rs = 0.0

print("Bellhop - Ellipsoidal boundaries:")
print("Coherent TL calculation")
print("Geometric gaussian beams")

fid = open('EllipseTLGB.ati','r')
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

fid = open('EllipseTLGB.bty','r')
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

system("bellhop.exe EllipseTLGB")

filename = 'EllipseTLGB.shd'
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

figure(1)
imshow(tl,extent=[0,rmaxkm,-Dmax,Dmax],aspect='auto',cmap='jet_r',origin='lower',vmin=60,vmax=120)
plot(ratikm,-zati,'b')
plot(rbtykm,zbty,'k')
cb = colorbar()
cb.ax.invert_yaxis()
plot(rs,zs,marker="<",markersize=16,color="k")
xlabel('Range (km)')
ylabel('Depth (m)')
title('Bellhop - Ellipsoidal boundaries')
xlim(0,rmaxkm)
ylim(Dmax,-Dmax)

show()

print("done.")
