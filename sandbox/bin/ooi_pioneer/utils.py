import xarray as xr
from numpy import abs, arange, unique
from os.path import exists

def readOOI (fname):
    """ 
    Update parameter names from OOI to iPROF
    Parameters
    ----------
    fname : str
        path of OOI Pioneer Profiler mooring downloaded data
    Returns
    -------
    ds : xarray DataSet
        dataset with iPROF variable names 
    """
    
    if ( exists(fname) ):
        ds = xr.open_dataset( fname )
    else:
        raise TypeError(f'Domain {fname} not recognized')

    ds = ds.rename({    'sea_water_temperature_profiler_depth_enabled': 'prof_T',
                        'sea_water_temperature_profiler_depth_enabled_qc_agg':'prof_Tflag',
                        'sea_water_practical_salinity_profiler_depth_enabled': 'prof_S',
                        'sea_water_practical_salinity_profiler_depth_enabled_qc_agg':'prof_Sflag',}) \
            .drop({     'sea_water_density_profiler_depth_enabled',
                        'sea_water_density_profiler_depth_enabled_qc_agg',
                        'sea_water_pressure_profiler_depth_enabled',
                        'sea_water_pressure_profiler_depth_enabled_qc_agg',})
    ds['prof_depth'] = xr.DataArray(-1 * ds.z, dims=ds.z.dims, attrs=ds.z.attrs)
    ds=ds.drop('z')
    
    writeTimeVariables (ds)
    #ds=addDepthDimension (ds)
    (ds['iPROF'], counts) = unique(ds.time, return_counts=True)
    return ds

def addDepthDimension (ds):
    """ 
    Create depth coordinate
    Parameters
    ----------
    ds : xarray DataSet
        OOI Profiler mooring data for one profiler
    Returns
    -------
    ds : xarray DataSet
        dataset with iDEPTH coordinate set as a dimension
    """
    if ( 'prof_depth' not in ds ): 
        raise TypeError('Couldn\'t find prof_depth data variable')
    if ( 'actual_range' not in ds.prof_depth.attrs ):
        raise TypeError('Couldn\'t find prof_depth range attribute')

    iDEPTH = arange(max(abs(ds.prof_depth.attrs['actual_range'])) + 1)
    return ds.expand_dims({"iDEPTH":iDEPTH})
    

def writeTimeVariables (ds):
    """ 
    Update from datetime64 times to default iPROF time
    Parameters
    ----------
    ds : xarray DataSet
        OOI Profiler mooring data for one profiler
    Returns
    -------
    None : adds data variables to existing DataSet
        dataset with iPROF time dataArrays 
    """
    times = ds.time.values.astype('datetime64[s]')
    yyyymmdd = []; hhmmss = []
    for time in times:
        tmp = str(time).partition('T')
        yyyymmdd.append( float(''.join( tmp[0].split("-", maxsplit=2) )) )
        hhmmss.append( float(''.join( tmp[-1].split(":", maxsplit=2) )) )
        
    ds['prof_YYYYMMDD'] = xr.DataArray(
                            yyyymmdd, dims=["row"],
                            attrs={'long_name' : 'year (4 digits), month (2 digits), day (2 digits)'}
                        )
    ds['prof_HHMMSS'] = xr.DataArray(
                            hhmmss, dims=["row"],
                            attrs={'long_name' : 'hour (2 digits), minute (2 digits), second (2 digits)'}
                        )
    return None
