from scipy.io import loadmat
from numpy import abs, cos, sqrt, arctan, sin, arccos, pi, tan,\
                  vectorize, exp, zeros, array
from pyproj import Geod

def loadMatFile ( file ):
    return loadmat(file)

def degMinSec2decimal (degree, minutes=0.0, seconds=0.0):
    return degree + minutes/60 + seconds/3600

def meter2deg (m):
    return m / 6371009. * 180 / pi

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

def wgs84Distance( lonA, latA, lonB, latB ):
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

def utm2wgs(x, y, utmzone):    
    # Converts arrays of UTM coordinates into Lon/Lat arrays.
    # Inputs:
    #    x       - UTM easting in meters
    #    y       - UTM northing in meters
    #    utmzone - UTM longitudinal zone
    # Outputs:
    #    lats (WGS84 Latitude vector)  in decimal degrees:  ddd.dddddddd
    #    lons (WGS84 Longitude vector) in decimal degrees:  ddd.dddddddd
    #
    # Example:
    #     x=[ 458731;  407653;  239027;  362850];
    #     y=[4462881; 3171843; 4302285; 2772478];
    #     utmzone=['30T'; '32T'; '01S'; '51R'];
    #     lons,lats=utm2wgs(x,y,utmzone);
    #       returns
    # lats =
    #    40.3154
    #    28.6705
    #    38.8307
    #    25.0618
    # lons =
    #    -3.4857
    #     8.0549
    #  -180.0064
    #   121.6403
    #
    # Source: DMA Technical Manual 8358.2, Fairfax, VA

    #input check
    n = len(x)
    if n != len(y) or n != len(utmzone) or len(y) != len(utmzone):
        raise IndexError('Inputs must be equal lengths')

    lenutm = vectorize(len)
    if any(lenutm(utmzone) !=3):
        raise ValueError('utmzone must contain strings with  3 characters only')

    ## init output and fixed parameters
    lons = zeros(n)
    lats = zeros(n)

    sa = 6378137.0      #WGS84 major exis of Earth [m]
    sb = 6356752.314245 #WGS84 minor axis of Earth [m]
    e = sqrt(sa**2 - sb**2) / sb # squared second eccentricity
    e2 = e*e
    c = sa*sa / sb
    scaleFac = 0.9996   #scale factor at central meridian
    fEasting = 500000.0 #false easting 

    #lon lat calc
    for i in range(n):
        if ( utmzone[i][-1] > 'X' or utmzone[i][-1] < 'C' ):
            raise ValueError('utmzone letter is invlaid')
        if utmzone[i][-1] > 'M':
            hemis = 'N'
        else:
            hemis = 'S'

        zone = float(utmzone[i][:2])
        X = x[i] - fEasting
        if hemis == 'N':
            Y = y[i]
        else:
            Y = y[i] - 10000000.

        lat      = Y / (6366197.724*scaleFac) 
        v        = scaleFac * c / sqrt(1. + e2*cos(lat)**2)
        a        = X/v
        a1       = sin(2.*lat)
        a2       = a1*cos(lat)**2
        j2       = lat + a1/2.
        j4       = ( 3*j2 + a2 ) / 4.
        j6       = ( 5*j4 + a2*cos(lat)**2 ) / 3.
        alpha    = e2 * 3 / 4.
        beta     = alpha**2 * 5 / 3.
        gamma    = alpha**3 * 35 / 27.
        Bm       = scaleFac*c*( lat - alpha*j2 + beta*j4 - gamma*j6 )
        b        = ( Y-Bm ) / v
        Epsi     = e2*a**2*cos(lat)**2 / 2.
        Eps      = a*( 1 - Epsi/3. )
        nab      = b*( 1 - Epsi ) + lat
        senoheps = ( exp(Eps) - exp(-1*Eps) ) / 2.
        Delta    = arctan( senoheps / cos(nab) )
        Ta0      = arctan( cos(Delta)*tan(nab) )

        lat      = lat + ( 1 + e2*cos(lat)**2 \
                   - 3*e2*sin(lat)*cos(lat)*(Ta0-lat)/2.)*(Ta0-lat)
        lats[i]  = lat*180/pi
        lons[i]  = zone*6. - 183. + Delta*180/pi

    return lons, lats
