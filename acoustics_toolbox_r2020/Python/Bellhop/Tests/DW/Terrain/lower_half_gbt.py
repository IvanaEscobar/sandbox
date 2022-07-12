#==================================================================
#  
#  Bellhop: Lower halfspace, Gaussian beams
#  Mexilhoeira Grande, Dom Jul 30 19:56:15 WEST 2017
#  Written by Tordar
#  
#==================================================================

# ipython: run lower_half_gbt

from os import *
import sys
from numpy import *
from scipy.io import *
from pylab import *
sys.path.append ("/home/orodrig/FORdoc/at/Python/")
from readshd import *

rs = 0.0

print("Bellhop - Lower halfspace, Gaussian beams:") 
print("Coherent TL calculation")
print("Geometric gaussian beams")

system("bellhop.exe lower_half_gbt")

filename = 'lower_half_gbt.shd'
xs = nan
ys = nan
pressure,geometry = readshd(filename,xs,ys)

zs     = geometry["zs"]
rarray = geometry["rarray"]; rarraykm = rarray/1000
zarray = geometry["zarray"]

Dmax = zarray[-1]
rmax = rarray[-1]; rmaxkm = rmax/1000

p = squeeze( pressure )
tl = -20*log10( abs( p ) )

figure(1)
plot(rarraykm,tl)
xlabel('Range (km)')
ylabel('TL (dB)')
title('Bellhop - Lower halfspace, Gaussian beams')
ylim(120,60)
grid(True)

show()

print("done.")
