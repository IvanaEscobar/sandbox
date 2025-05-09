#=======================================================================
# 
# Bellhop: Block problem
# Mexilhoeira Grande, Dom Dez 25 22:07:45 WET 2016
# Written by Tordar 
# 
#=======================================================================

from os import system
from numpy import *
from scipy.io import *
from pylab import *
from wbellhopenvfil import *
from plotray import *

# commandline: python bellhop_block_rays.py 

case_title = "Block problem"

freq   = 100.0
Rmaxkm =   5.0; Rmax = Rmaxkm*1000.0 
Dmax   = 1000.0 
cw   = 1500.0 # sound speed in water
cb   = 1700.0 # sound speed in lower halfspace
rhow =  1.0   # density in water
rhob =  2.0   # density in lower halfspace

source_nrays    =  301 # number of propagation rays considered #
source_aperture = 20.0 # maximum launching angle (degrees) #
source_ray_step =  5.0 # ray calculation step (meters) #

#==================================================================
#  
#  Source properties 
#  
#==================================================================

nzs  = 1
zs   = array([500.0])
rs   = array([  0.0])
zbox = 1001.0 
rbox = 5.1  # km!!!!!
box  = array([source_ray_step,zbox,rbox])
thetas = array([source_nrays,-source_aperture,source_aperture]) 
p      = zeros(1)
comp   = ''

source_data = {"zs":zs, "box":box, "f":freq, "thetas":thetas, "p":p, "comp":comp}

#==================================================================
#  
#  Surface definition:
#  
#==================================================================

itype = ''
xati  = []  # The *.ati file won't be written  
p     = []  # Surface properties
aunits= ''

surface_data = {"itype":itype,"x":xati,"p":p,"units":aunits}

#==================================================================
#  
#  Sound speed:
#  
#==================================================================

z = array([0.0,Dmax])
c = array([cw,cw])
r = []

ssp_data = {"r":r,"z":z,"c":c}

#==================================================================
#  
#  Bottom:
#  
#==================================================================

rbty   = array([rs[0]-2,2000,2010,2990,3000,Rmax+2]); rbtykm = rbty/1000.0
zbty   = array([Dmax,Dmax,500,500,Dmax,Dmax])
itype  = '''L''' # RID properties
bunits = '''W'''
xbty   = array([rbtykm,zbty]) # Bottom coordinates
p      = array([2000.0,0.0,2.0,0.5,0.0]) # Bottom properties

bottom_data = {"itype":itype,"x":xbty,"p":p,"units":bunits}

#==================================================================
#  
#  Array: 
#  
#==================================================================

options1    = '''CVW''' # No ati file expected  
options2    = '''A*'''
options3    = '''R'''
options4    = []

rarray = zeros(1); rarraykm = zeros(1)
zarray = zeros(1)

options = {"options1":options1,"options2":options2,"options3":options3,"options4":options4,"rarray":rarraykm,"zarray":zarray}

print("Writing environmental file...")

wbellhopenvfil('block',case_title,source_data,surface_data,ssp_data,bottom_data,options)

print( "Running Bellhop..." )

system("bellhop.exe block")

print( "Reading output data..." )

figure(1)
plotray('block.ray')
plot(rbty,-zbty,'k',linewidth=2)
xlabel('Range (m)')
ylabel('Depth (m)')
title('Bellhop - Block problem')

show()

print("done.")

