#=======================================================================
# 
# Bellhop: Calibration case with gradient
# Mexilhoeira Grande, Dom Jul 30 21:16:01 WEST 2017
# Written by Tordar 
# 
#=======================================================================

from os import system
from numpy import *
from scipy.io import *
from pylab import *
sys.path.append ("/home/orodrig/FORdoc/at/Python/")
from plotray import *

print("Bellhop - Calibration case with gradient:") 
print("Ray trace run")
print("Geometric hat beams in Cartesian coordinates")

system("bellhop.exe calibraygrad")

figure(1)
plotray('calibraygrad.ray')
xlabel('Range (m)')
ylabel('Depth (m)')
title("Bellhop - Calibration case with gradient")

show()

print("done.")
