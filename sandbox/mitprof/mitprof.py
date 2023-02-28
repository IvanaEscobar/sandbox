import xarray as xr
from os.path import exists

def MITprof_read ( fname ):
    """ 
    Read NetCDF iPROF full data into xarray DataSet
    Parameters
    ----------
    fname : str
        data file name
    Returns
    -------
    ds : xarray DataSet
        dataset containing profiler fields
    """

    if ( exists(fname) ):
        ds = xr.open_dataset( fname )
    else:
        raise TypeError(f'Domain {fname} not recognized')

    return ds
