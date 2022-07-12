#=======================================================================
# 
# Bellhop: Dickins Seamount
# Mexilhoeira Grande, Dom Jul 30 11:42:34 WEST 2017
# Written by Tordar 
# 
#=======================================================================

from os import system
from numpy import *
from scipy.io import *
from pylab import *
sys.path.append ("/home/orodrig/FORdoc/at/Python/")
from plotray import *

print("Bellhop - Dickins Seamount:") 
print("Ray trace run")
print("Geometric hat beams in Cartesian coordinates") 

system("bellhop.exe DickinsFlatBray")

figure(1)
plotray('DickinsFlatBray.ray')
xlabel('Range (m)')
ylabel('Depth (m)')
title("Bellhop - Dickins Seamount")

show()

print("done.")

