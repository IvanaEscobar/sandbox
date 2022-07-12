#=======================================================================
# 
# Bellhop: Block ray case
# Faro, Qua Dez  7 20:11:51 WET 2016
# Written by Tordar 
# 
#=======================================================================

# python       : python  blockB_ray.py
# python prompt: execile('blockB_ray.py') 
# ipython      : run blockB_ray

from os import system
from numpy import *
from scipy.io import *
from pylab import *
sys.path.append ("/home/orodrig/FORdoc/Traceo/Python/")
from plotray import *

print("Bellhop - block case")

fid   = open('blockB_ray.bty','r')
itype = fid.readline()
n     = int( fid.readline() )
rbtykm = zeros(n)
zbty   = zeros(n)
for i in range(n):
    theline = str(fid.readline())
    datai = theline.split()
    rbtykm[i] = float( datai[0] )
    zbty[  i] = float( datai[1] )
fid.close()

rbty = rbtykm*1000

system('bellhop.exe blockB_ray')

figure(1)
plotray('blockB_ray.ray')
plot(rbty,-zbty,'k',linewidth=2)
show()

print("done.")
