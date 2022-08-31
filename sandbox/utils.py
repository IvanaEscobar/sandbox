from scipy.io import loadmat
from numpy import abs, cos, sqrt, arctan, sin, arccos, pi

def loadMatFile ( file ):
    return loadmat(file)

def degMinSec2decimal (degree, minutes=0.0, seconds=0.0):
    return degree + minutes/60 + seconds/3600

def gcDistance( latA, lonA, latB, lonB ):
# Great circle distance is the shortest distance between two points on a sphere
# Using the Vicenty formula for an ellipsoid with equal major and minor axes
# Lats and lons are in degrees
# Distance is in meters
    radius = 6371009.0 # [m] mean Earth radius
    latA = latA * pi/180.
    latB = latB * pi/180.
    dlon = abs(lonB - lonA) * pi/180. 

    numer = ( cos(latB)*sin(dlon) )**2 + \
            ( cos(latA)*sin(latB) - sin(latA)*cos(latB)*cos(dlon) )**2
    denom = sin(latA)*sin(latB) + cos(latA)*cos(latB)*cos(dlon)

    return radius * arctan( sqrt(numer) / denom )

def wgs84Distance( latA, lonA, latB, lonB ):
# For the WGS84 spheroid up to 3 terms in lat and 2 terms in lon
# Lats and lons are in degrees
# Distance is in meters
    midLat = (latA + latB) / 2.
    dlat = abs(latB - latA)
    dlon = abs(lonB - lonA) 

    latM = 111132.953 - 559.850*cos( 2 * midLat ) + 1.175*cos( 4 * midLat)
    lonM = 111412.877*cos( midLat ) - 93.504*cos( 3*midLat )

    return sqrt( (dlat*latM)**2 + (dlon*lonM)**2 ) 
