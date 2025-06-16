from scipy.io import loadmat
from numpy import abs, cos, sqrt, arctan, sin, arccos, pi, tan,\
                  vectorize, exp, zeros, array, reshape, fromfile,\
                  unique, datetime64
from numpy import issubdtype as _issubdtype
from pyproj import Geod
from xarray import concat as _concat

def loadMatFile ( file ):
    return loadmat(file)

def degMinSec2decimal (degree, minutes=0.0, seconds=0.0):
    return degree + minutes/60 + seconds/3600

def meter2deg (m):
    return m / 6371009. * 180 / pi

def lon180to360 (lon):
    # Input: longitude that spans [-180 180] degE or [180 degW 180degE]
    # Output: longitude that spans [0 360] deg'E'
    #
    # Note: 0 deg lies at Greenwich, England aka the international date line 
    return lon % 360.

def lon360to180 (lon):
    # Input: longitude that spans [0 360] deg'E'
    # Output: longitude that spans [-180 180] degE or [180 degW 180degE]
    #
    # Note: 0 deg lies at Greenwich, England aka the international date line 
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

    latM = 111132.953 - 559.850*cos( 2*midLat ) + 1.175*cos( 4*midLat )
    lonM = 111412.877*cos( midLat ) - 93.504*cos( 3*midLat )

    return sqrt( (dlat*latM)**2 + (dlon*lonM)**2 ) 

def wgs84space(start, end, npts=3):
    # start = [startlon, startlat]
    # end =   [endlon,   endlat]
    g = Geod(ellps='WGS84')

    startlon = start[0]
    startlat = start[1]
    endlon   = end[0]
    endlat   = end[1]

    inpts = npts - 2
    if inpts<=0:
        raise ValueError('Not enough points in the line')

    # calculate distance between points
    r = g.inv_intermediate(startlon, startlat, endlon, endlat, inpts, return_back_azimuth=True)

    # npts doesn't include start/end points, so prepend/append them
    r.lons.insert(0, startlon)
    r.lats.insert(0, startlat)
    r.lons.append(endlon)
    r.lats.append(endlat)

    return r 

def wgs84pointBtwnAB(start, end, r_btwn):
    # start = [startlon, startlat]
    # end   = [endlon,   endlat]
    # r_btwn = distance to a midpoint along A to B geodesic in METERS
    # Return: 
    # btwn  = [btwnlon,  btwnlat]
    g = Geod(ellps='WGS84')

    startlon = start[0]
    startlat = start[1]
    endlon   = end[0]
    endlat   = end[1]

    # calculate azimuth of geodesic
    azi,b,d = g.inv(startlon, startlat, endlon, endlat)

    # use range to find lat, lon of point in between A and B
    btwnlon, btwnlat, b = g.fwd(startlon, startlat, azi, r_btwn)

    return [btwnlon,btwnlat]

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
    arrShape = x.shape
    if x.ndim==2 or y.ndim==2 or utmzone.ndim==2:
        x = x.flatten()
        y = y.flatten()
        utmzone = utmzone.flatten()

    n = x.size
    if n <= 1:
        raise TypeError('provide an array for all inputs')
    if n != y.size or n != utmzone.size or y.size != utmzone.size:
        raise IndexError('Inputs must be equal lengths')

    lenutm = vectorize(len)
    if any(lenutm(utmzone) !=3):
        raise ValueError('utmzone must contain strings with 3 characters only')

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

    lons = reshape(lons, arrShape)
    lats = reshape(lats, arrShape)
    return lons, lats

def loadmitgcmbinary(ff, shape, dtype='>f4'):
    # binary shape from MITgcm will ALWAYS be in order of
    # 3D: nz,ny,nx 
    # 2D: ny,nx
    # This fxn will return the binary in it's native ordering
    # Return: data

    data = fromfile(ff, dtype=dtype).reshape(shape)
    return data

def concatDs(manyDs, grid=None, merge=True):
    '''
    Assuming a repeated T is identical between datasets, 
    we concatenate only the first instance of a unique T coordinate
    '''
    uniqueT = []
    uniqueDs = []

    for ds in manyDs:
        for T in ds.T.data:
            oneT = ds.sel(T=T)
            if T not in uniqueT:
                uniqueT.append(T)
                uniqueDs.append(oneT)

    ds = _concat( uniqueDs, dim='T' )
    ds = renameDs( ds )
    if not _issubdtype(ds.T.dtype, datetime64):
        ds.coords['years'] = ds.T/3600/24/360.
    else: 
        ds.coords['years'] = ds.iter*120/3600/24/360.
    
    if merge and (grid is not None):
        if len(ds.Z.data) == 1:
            ds = ds.squeeze(dim='Z')
        ds = ds.merge( grid )
    
    return ds

def check_uniqueT( T ):
    print( len(T) - len(unique(T)) )
    return None

def renameDs( ds ):
    '''
    Hardcoding renaming dimensions generated using gluemnc bash script
    '''
    
    myDims = {}
    for dim in ds.dims:
        dimName = dim[0:3]
        
        if dimName == 'Zd0' or dimName == 'Zmd':
            myDims[dim] = 'Z'
        elif dimName == 'Zld':
            myDims[dim] = 'Zl'
        elif dimName == 'Y':
            myDims[dim] = 'YC'
        elif dimName == 'X':
            myDims[dim] = 'XC'
        elif dimName == 'Yp1':
            myDims[dim] = 'YG'
        elif dimName == 'Xp1':
            myDims[dim] = 'XG'    

    if 'iter' not in ds.coords:
        ds=ds.set_coords('iter')

    return ds.rename(myDims)

def add_SRpoints(ax, line, **plot_kwargs):
    '''
    Add scatter points to an Axes object, such as one from a subplot or xarray plot.

    Parameters:
        ax : matplotlib.axes.Axes
            The axes to draw on. Can be directly from subplots or from .plot().axes
        line : Geod object
            Must have attributes `lons` and `lats` as arrays.
        **plot_kwargs : optional
            Additional arguments passed to `ax.scatter`.
    '''

    ax.scatter(line.lons[0],  line.lats[0],  c='C0', **plot_kwargs)
    ax.scatter(line.lons[-1], line.lats[-1], c='C1', **plot_kwargs)

    return None
