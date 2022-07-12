#==================================================================
#  
#  Bellhop: Dickins sound speed profile
#  Mexilhoeira Grande, Dom Jul 30 11:49:20 WEST 2017
#  Written by Tordar
#  
#==================================================================

# ipython: run DickinsFlatB

from os import system
from numpy import *
from scipy.io import *
from pylab import *
sys.path.append ("/home/orodrig/FORdoc/at/Python/")
from readshd import *

rs = 0.0

print("Bellhop - Dickins Seamount:")
print("Coherent TL calculation")
print("Geometric gaussian beams") 

system("bellhop.exe DickinsFlatB")

filename = 'DickinsFlatB.shd'
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
imshow(tl,extent=[0,rmaxkm,0,Dmax],aspect='auto',cmap='jet_r',origin='lower',vmin=60,vmax=120)
cb = colorbar()
cb.ax.invert_yaxis()
plot(rs,-zs,marker="<",markersize=16,color="k")
xlabel('Range (km)')
ylabel('Depth (m)')
title('Bellhop - Dickins Seamount, 230 Hz')
ylim(Dmax,0)

show()

print("done.")
