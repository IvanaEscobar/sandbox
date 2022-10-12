from numpy import abs
from xmitgcm import llcreader
import xarray as xr
import sandbox as sb

aste = llcreader.SverdrupASTE270Model()
asteDs = aste.get_dataset(k_chunksize=90)
ds = asteDs.sel(face=2, k=0).isel(time=0)
qq = []

for i in ds.i[0:-1]:
    for j in ds.j[0:-1]:
        if not ds.XC[j,i].isnull():
            qq.append(sb.gcDistance(ds.YC[j,i], ds.XC[j,i],\
                        ds.YC[j+1, i+1], ds.XC[j+1, i+1]).values )
            
print(qq)
