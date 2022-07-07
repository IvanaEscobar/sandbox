from scipy.io import loadmat
from numpy import abs, cos, sqrt

def loadMatFile ( file ):
    return loadmat(file)

def degMinSec2decimal (degree, minutes=0.0, seconds=0.0):
    return degree + minutes/60 + seconds/3600

def wgs84Distance( latA, lonA, latB, lonB ):
# For the WGS84 spheroid up to 3 terms in lat and 2 terms in lon
    midLat = (latA + latB) / 2.
    dlat = abs(latB - latA)
    dlon = abs(lonB - lonA) 

    latM = 111132.953 - 559.850*cos( 2 * midLat ) + 1.175*cos( 4 * midLat)
    lonM = 111412.877*cos( midLat ) - 93.504*cos( 3*midLat )

    return sqrt( (dlat*latM)**2 + (dlon*lonM)**2 ) 
