#==================================================================
#  
#  Bellhop: Shaded point source in free space
#  Mexilhoeira Grande, Sab Jul 29 13:54:34 WEST 2017
#  Written by Tordar
#  
#==================================================================

# ipython: run shaded

from os import system
from numpy import *
from scipy.io import *
from pylab import *
sys.path.append ("/home/orodrig/FORdoc/at/Python/")
from readshd import *

rs = 0.0

print("Bellhop - Shaded point source in free space")

fid = open('shaded.sbp','r')
theline = fid.readline()
n       = int( theline )
angles  = zeros( n ) 
powerdB = zeros( n )
for i in range(n):
    theline = str( fid.readline() )
    datai = theline.split()
    angles[ i] = float( datai[0] )
    powerdB[i] = float( datai[1] )
fid.close()
anglesRad = angles*pi/180

system("bellhop.exe shaded")

filename = 'shaded.shd'
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
polar( anglesRad, abs(powerdB) )

figure(2)
imshow(tl,extent=[-rmaxkm,rmaxkm,-Dmax,Dmax],aspect='auto',cmap='jet_r',origin='lower',vmin=40,vmax=90)
cb = colorbar()
cb.ax.invert_yaxis()
plot(rs,zs,marker="<",markersize=16,color="k")
xlabel('Range (km)')
ylabel('Depth (m)')
title('Bellhop - Shaded point source in free space')
xlim(0,rmaxkm)
ylim(Dmax,0)

show()

print("done.")
