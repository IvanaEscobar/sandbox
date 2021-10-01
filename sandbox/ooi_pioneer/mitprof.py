import xarray as xr
from os.path import exists

def MITprof_read ( fname ):
    """ 
    read netcdf iPROF data file into python
    Parameters
    ----------
    fname : str
        file data name
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
