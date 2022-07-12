#=======================================================================
# 
# Bellhop: Munk profile
# Mexilhoeira Grande, Dom Jul 30 19:18:24 WEST 2017
# Written by Tordar 
# 
#=======================================================================

from os import system
from numpy import *
from scipy.io import *
from pylab import *
sys.path.append ("/home/orodrig/FORdoc/at/Python/")
from plotray import *

print("Bellhop - Munk profile:") 
print("Ray trace run")
print("Geometric hat beams in Cartesian coordinates")

system("bellhop.exe MunkB_ray")

figure(1)
plotray('MunkB_ray.ray')
xlabel('Range (m)')
ylabel('Depth (m)')
title("Bellhop - Munk profile")

show()

print("done.")
