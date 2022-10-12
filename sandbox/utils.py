from scipy.io import loadmat
from numpy import abs, cos, sqrt, arctan, sin, arccos, pi
from pyproj import Geod

def loadMatFile ( file ):
    return loadmat(file)

def degMinSec2decimal (degree, minutes=0.0, seconds=0.0):
    return degree + minutes/60 + seconds/3600

def lon180to360 (lon):
    return lon % 360.

def lon360to180 (lon):
    return (lon+180.) % 360. - 180.

def gcDistance( lonA, latA, lonB, latB ):
# Great circle distance is the shortest distance between two points on a sphere
# Using the Vincenty formula for an ellipsoid with equal major and minor axes
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

def gcspace(start, end, npts=2):
    # start = [startlon, startlat]
    # end =   [endlon,   endlat]

    startlong = start[0]
    startlat  = start[1]
    endlong   = end[0]
    endlat    = end[1]

    # calculate distance between points
    g = Geod(ellps='WGS84')
    (az12, az21, dist) = g.inv(startlong, startlat, endlong, endlat)

    interiorpts = npts - 2
    if interiorpts<0:
        print("ERROR")
        return None 
    elif interiorpts==0:
        # calculate line string along path with segments <= 1 km
        npts = 1 + int(dist / 1000)
    lonlats = g.npts(startlong, startlat, endlong, endlat, interiorpts)

    # npts doesn't include start/end points, so prepend/append them
    lonlats.insert(0, (startlong, startlat))
    lonlats.append((endlong, endlat))

    lons=[]; lats=[]
    for (lon, lat) in lonlats:
        lons.append(lon)
        lats.append(lat)
                                
    return [lons, lats]
