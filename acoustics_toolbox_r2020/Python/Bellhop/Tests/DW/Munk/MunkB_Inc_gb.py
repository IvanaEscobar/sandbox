#==================================================================
#  
#  Bellhop: Munk profile, incoherent, Gaussian beam 
#  Mexilhoeira Grande, Dom Jul 30 19:13:01 WEST 2017
#  Written by Tordar
#  
#==================================================================

# ipython: run MunkB_Inc_gb

from os import *
import sys
from numpy import *
from scipy.io import *
from pylab import *
sys.path.append ("/home/orodrig/FORdoc/at/Python/")
from readshd import *

rs = 0.0

print("Munk profile, incoherent, Gaussian beam:") 
print("Coherent TL calculation")
print("Geometric gaussian beams")

system("bellhop.exe MunkB_Inc_gb")

filename = 'MunkB_Inc_gb.shd'
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
title('Munk profile, incoherent, Gaussian beam')
ylim(Dmax,0)

show()

print("done.")
