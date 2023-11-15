import xarray as xr
import xmitgcm
from xesmf import Regridder
from numpy import vectorize, exp, zeros, array, arange, dtype

def generateBathyBinary (gridDir,nj,ni, regional=True):
    
    # Load GEBCO bathymetry as xr.DataSet 
    gebco = xr.open_dataset('/scratch2/shared/gebco22/GEBCO_2022_sub_ice_topo.nc')

    # Load gcm grid cell centers to be used for regridding
    xc = xmitgcm.utils.read_raw_data( gridDir+'XC.data', shape=(nj,ni), dtype=dtype('>f4') )
    yc = xmitgcm.utils.read_raw_data( gridDir+'YC.data', shape=(nj,ni), dtype=dtype('>f4') )

    # Create lat, lon xr.DataArrays
    lon = xr.DataArray( array(xc, dtype='float32'), dims=('j','i'), 
                        coords={'j': arange(nj),
                                'i': arange(ni),}
                      )
    lat = xr.DataArray( array(yc, dtype='float32'), dims=('j','i'), 
                        coords={'j': arange(nj),
                                'i': arange(ni),}
                      )

    # Create gcm grid xr.Dataset
    grid = xr.Dataset({'lon':lon, 'lat':lat})

    # GEBCO is massive, use a subset as default
    if regional:
        latMin=yc.min()
        latMax=yc.max()
        lonMin=xc.min()
        lonMax=xc.max()
        gebco = gebco.sel(  lat=slice(latMin,latMax),
                            lon=slice(lonMin,lonMax) )
    else:
        return None
        # GLOBAL

    # downscaling means we can use a cheap interpolation: longest computation
    regridder = Regridder(gebco.elevation, grid, 'nearest_s2d')
    Depth = regridder(gebco.elevation)
     
    return Depth

