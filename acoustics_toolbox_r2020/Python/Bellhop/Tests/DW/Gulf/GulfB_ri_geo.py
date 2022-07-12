#==================================================================
#  
#  Bellhop: Topgulf, Range-ind., Geometric beams  
#  Mexilhoeira Grande, Dom Jul 30 14:30:20 WEST 2017
#  Written by Tordar
#  
#==================================================================

# ipython: run GulfB_ri_geo

from os import system
from numpy import *
from scipy.io import *
from pylab import *
sys.path.append ("/home/orodrig/FORdoc/at/Python/")
from readshd import *

rs = 0.0

print("Bellhop - Topgulf, Range-ind., Geometric beams:")
print("Coherent TL calculation")
print("Geometric hat beams in Cartesian coordinates")

system("bellhop.exe GulfB_ri_geo")

filename = 'GulfB_ri_geo.shd'
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
tl[tl > 120.0] = 120.0

figure(1)
imshow(tl,extent=[0,rmaxkm,0,Dmax],aspect='auto',cmap='jet_r',origin='lower',vmin=60,vmax=120)
cb = colorbar()
cb.ax.invert_yaxis()
plot(rs,zs,marker="<",markersize=16,color="k")
xlabel('Range (km)')
ylabel('Depth (m)')
title('Bellhop - Topgulf, Range-ind., Geometric beams')
xlim(0,rmaxkm)
ylim(Dmax,0)

show()

print("done.")
