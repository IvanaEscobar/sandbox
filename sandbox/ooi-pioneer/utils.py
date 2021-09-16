import xarray as xr

def readOOI (fname):
    """ Update parameter names from OOI to iPROF

    Parameters
    ----------
    fname : str
        path of OOI Pioneer Profiler mooring downloaded data
    Returns
    -------
    ds : xarray DataSet
        dataset with iPROF variable names 
    """
    
    ds = xr.open_dataset(fname)
    ds = ds.rename({    'row':'iPROF', 
                        'sea_water_temperature_profiler_depth_enabled': 'prof_T',
                        'sea_water_temperature_profiler_depth_enabled_qc_agg':'prof_Tflag',
                        'sea_water_practical_salinity_profiler_depth_enabled': 'prof_S',
                        'sea_water_practical_salinity_profiler_depth_enabled_qc_agg':'prof_Sflag',})\\
            .drop({     'sea_water_density_profiler_depth_enabled',
                        'sea_water_density_profiler_depth_enabled_qc_agg',
                        'sea_water_pressure_profiler_depth_enabled',
                        'sea_water_pressure_profiler_depth_enabled_qc_agg',})
    ds['prof_depth'] = xr.DataArray(-1 * ds.z, dims=ds.z.dims, attrs=ds.z.attrs)
    ds=ds.drop('z')

    return
