#=======================================================================
# 
# Bellhop: Munk profile
# Mexilhoeira Grande, Sab Jul 29 14:19:39 WEST 2017
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
print("Eigenray trace run")
print("Geometric hat beams in Cartesian coordinates")

system("bellhop.exe MunkB_eigenray")

figure(1)
plotray('MunkB_eigenray.ray')
xlabel('Range (m)')
ylabel('Depth (m)')
title("Bellhop - Munk profile")

show()

print("done.")

