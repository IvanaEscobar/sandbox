#!/bin/bash python
# External imports
import arlpy.uwapm as _pm
from numpy import datetime64 as _datetime64
# Internal imports
from ..utils import wgs84fromBearing, wgs84space

# Basic utils for bcg.hd
lineSource = lambda ds, line: ds.sel(YC=line.lats[0],  XC=line.lons[0],  method='nearest')
lineReceiv = lambda ds, line: ds.sel(YC=line.lats[-1], XC=line.lons[-1], method='nearest')
ihop = _pm._Bellhop()

# set-up diagnostics
ts080 = list(range(21168000, 21189601, 30))
ts100 = list(range(26337600, 26359201, 30))

# Distance in meters and bearing in degrees
distance = 400000  # 400 km
bearing  = 0       # 0 degrees (northwards)
line_M080 = wgs84fromBearing([14, 47], bearing, distance, npts=25)
line_M100 = wgs84fromBearing([18, 47], bearing, distance, npts=25)

line_Z080 = wgs84space([16, 56], [21.597, 56], npts=35)
line_Z100 = wgs84space([16, 59], [22.073, 59], npts=35)

for at in range(len(line_Z080.lats)):
    line_Z080.lats[at]=56
    line_Z100.lats[at]=59

### MITgcm variables ###
# data timestep size
dt = 120.
# data.cal startdate
startdate = _datetime64('1941-01-01 00:00:00')

# data.ihop::ihop_r[dr]
#           lon,       lat,       depth
m080_rvr = [14.000000, 47.001796, 160]
