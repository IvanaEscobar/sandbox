import xmitgcm
import numpy as np
import xarray as xr


def compare3D(da1, da2):
    da_diff = (da1 - da2).compute()
    nonzero = da_diff.where(da_diff != 0, drop=True)
    
    if nonzero.size == 0:
        print("MNC and MDSIO are equivalent for ", da1.standard_name)
        
    return None

def compare_mnc_mdsio(dataPath='./', mpi=False):
    
    # dataPath='/home/ivana/regionalgcm/baroclinic_gyre/run/run/'
    
    ds=xmitgcm.open_mdsdataset(dataPath+'diags', 
                                grid_dir=dataPath,
                                prefix=['dynDiag','ssp'])
    if mpi:
        mncPath='MNC_ALL/'
        t='glob'
    else:
        mncPath='mnc_test_0001/'
        t='t001'

    mncGRID = xr.load_dataset(dataPath+mncPath+f'grid.{t}.nc')
    mncSSP  = xr.load_dataset(dataPath+mncPath+f'ssp.0000000000.{t}.nc')
    mncDIAG = xr.load_dataset(dataPath+mncPath+f'dynDiag.0000000000.{t}.nc')

    mncSSP = mncSSP.rename({'Zmd000015':'Z', 'X':'XC', 'Y':'YC'})
    mncSSP = mncSSP.set_coords("iter")
    mncSSP = mncSSP.swap_dims({"T": "iter"})

    mncDIAG = mncDIAG.rename({'Zmd000015':'Z', 'X':'XC', 'Y':'YC'})
    mncDIAG = mncDIAG.set_coords("iter")
    mncDIAG = mncDIAG.swap_dims({"T": "iter"})

    ds = ds.swap_dims({"time": "iter"})

    # == 
    # compare some 3d fields
    # ==
    compare3D(ds.ihop_ssp, mncSSP.ihop_ssp)
    compare3D(ds.THETA, mncDIAG.THETA)
    
    return None