import xarray as xr
from numpy import array

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
                        'sea_water_practical_salinity_profiler_depth_enabled_qc_agg':'prof_Sflag',}) \
            .drop({     'sea_water_density_profiler_depth_enabled',
                        'sea_water_density_profiler_depth_enabled_qc_agg',
                        'sea_water_pressure_profiler_depth_enabled',
                        'sea_water_pressure_profiler_depth_enabled_qc_agg',})
    ds['prof_depth'] = xr.DataArray(-1 * ds.z, dims=ds.z.dims, attrs=ds.z.attrs)
    ds=ds.drop('z')

    return

def default_depth ():
    """ 
    Load default 85 depth levels
    Parameters
    ----------
    fname : 
        
    Returns
    -------
    depthLvls : numpy array
        85 depths in meters
    """
    depthsLvls=[5, 15, 25, 35, 45, 55, 65, 75, 85, 95, \
                105, 115, 125, 135, 145, 155, 165, 175, 185, 200, \
                220, 240, 260, 280, 300, 320, 340, 360, 380, 400, \
                420, 440, 460, 480, 500, 550, 600, 650, 700, 750, \
                800, 850, 900, 950, 1000, 1100, 1200, 1300, 1400, 1500, \
                1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, \
                2600, 2700, 2800, 2900, 3000, 3100, 3200, 3300, 3400, 3500, \
                3600, 3700, 3800, 3900, 4000, 4100, 4200, 4300, 4400, 4500, \
                4600, 4700, 4800, 4900, 5000]
    return array( depthLvls )
