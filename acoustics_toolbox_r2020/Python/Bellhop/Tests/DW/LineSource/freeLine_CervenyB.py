#==================================================================
#  
#  Bellhop: Free space, line source, Cerveny Gaussian beam
#  Mexilhoeira Grande, Dom Jul 30 16:28:16 WEST 2017
#  Written by Tordar
#  
#==================================================================

# ipython: run freeLine_CervenyB

from os import system
from numpy import *
from scipy.io import *
from pylab import *
sys.path.append ("/home/orodrig/FORdoc/at/Python/")
from readshd import *

rs = 0.0

print("Bellhop - Free space, line source, Cerveny Gaussian beam:")
print("Coherent TL calculation")
print("Ray centered beams")

system("bellhop.exe freeLine_CervenyB")

filename = 'freeLine_CervenyB.shd'
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
imshow(tl,extent=[0,rmaxkm,0,Dmax],aspect='auto',cmap='jet_r',origin='lower')
cb = colorbar()
cb.ax.invert_yaxis()
plot(rs,zs,marker="<",markersize=16,color="k")
xlabel('Range (km)')
ylabel('Depth (m)')
title('Bellhop - Free space, line source, Cerveny Gaussian beam')
xlim(0,rmaxkm)
ylim(Dmax,0)

show()

print("done.")
