#==================================================================
#  
#  Bellhop: Munk profile (Gaussian beam option)
#  Mexilhoeira Grande, Dom Jul 30 17:03:33 WEST 2017
#  Written by Tordar
#  
#==================================================================

# ipython: run MunkB_gb

from os import *
import sys
from numpy import *
from scipy.io import *
from pylab import *
sys.path.append ("/home/orodrig/FORdoc/at/Python/")
from readshd import *

rs = 0.0

print("Bellhop - Munk profile:") 
print("Coherent TL calculation")
print("Geometric gaussian beams")

system("bellhop.exe MunkB_gb")

filename = 'MunkB_gb.shd'
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
title('Bellhop - Munk profile (Gaussian beam option)')
ylim(Dmax,0)

show()

print("done.")
