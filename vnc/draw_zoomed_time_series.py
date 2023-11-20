import xmitgcm
import sandbox as sb
import numpy as np
import xarray as xr
from pandas import to_datetime

# for visualization
import cmocean.cm as cm
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import cartopy.crs as ccrs



def get_da_indices(da, myticks):
    # da is the i or j dimension as a 1d value
    # ds.YC.isel(j=30).compute().i for curvilinear grids
                
    inds=[]
    for tick in myticks:
        if (tick > da.min()) and (tick< da.max()):
            inds.append( np.where(da == tick)[0][0] )
        else:
            inds.append(0)
    return inds

def saveFrame4(idT, fname=''):
    fig,axs=plt.subplots(1,1, figsize=(10,3))

    # draw sample lines
    for xx in ds_expanded['nd'].data:
        axs.axvline(x=xx, color='gray', lw=0.2)
            
    # Draw a moving vertical line
    if idT>0:
        axs.axvline(x=ds_expanded.nd[idT].data, color='C7', lw=1.5)
        
    for zID,c in zip([0,10,20,27,40], ['C2','C3','C4','C5','C6']):
        if zID==0:
            ll = 'surface'
        elif zID>=40:
            ll='abyssal'
        else:
            ll=str(int(-1*stdt.Z.isel(k=zID).data))+ ' m'

            meanssp.sel(k=zID).plot(ax=axs, x='nd',  label=ll, lw=2,color=c)        

    axs.legend()
    axs.set_xlim(ds_expanded['nd'][0], ds_expanded['nd'][-1])
    axs.set_title('Horizontal mean sound speed by level', fontsize=18)
    axs.set_ylabel(r'$\bar{c}$ [m/s]', fontsize=14)
    axs.set_xlabel('date [yyyy-mm]', fontsize=14)
    axs.legend(loc='center right', fontsize=14)

    if fname:
        plt.savefig(fname, bbox_inches='tight', transparent=True, dpi=300)
    else:
        plt.show()
    plt.close()
    return None


# monthly means for 50 years
dataPath='/home/ivana/vnc/run/'
ds=xmitgcm.open_mdsdataset(dataPath+'diags', grid_dir=dataPath, prefix=['state3D','ssp'], geometry='curvilinear')

print('Loaded dataset')
imgPath='/home/ivana/regionalgcm/img/'

# save vnc DataSet
# remove pad
vnc=ds.isel(i=slice(9,238), i_g=slice(9,239),
            j=slice(11,199), j_g=slice(11,200),
            time=slice(1,-1) )

# nan on maskC
vnc = vnc.where(vnc.maskC,other=np.nan)

# nan on Land
vnc['Depth'] = vnc['Depth'].where(vnc.Depth!=0)

# save dates for vis titles
date_times = np.datetime64('2001-01-01T00:00:00') \
                + (vnc.iter*300).astype('timedelta64[s]')
vnc.coords['dates'] = ('time', 
to_datetime(date_times).strftime('%d/%m/%y %H:%M'))

lon_ticks = vnc.XC.isel(i=150).compute()
lat_ticks = vnc.YC.isel(j=30).compute()

# acoustic line
nLine = 4
line = sb.wgs84space([167.416672,-20.976704], [169.25,-19.539143], nLine)
lineID = [[128, 139],[148,138]] # in [J],[I] for ds.sel
myIters=[num for num in range(1174176, 1176335) if num%2==0]

# parse dataset in space
dsInterior = vnc.isel(i=slice(106,164), j=slice(107,157))
dsInterior.coords['nd'] = ( 'time', to_datetime(dsInterior.dates.compute().values, format='%d/%m/%y %H:%M') )

meanssp = dsInterior.ihop_ssp.mean(dim={'j','i'})
stdssp  = dsInterior.ihop_ssp.std(dim={'j','i'})
meant = dsInterior.THETA.mean(dim={'j','i'})
stdt  = dsInterior.THETA.std(dim={'j','i'})

#parse dataset in time, for the ihop experiment
# Convert 'iter' to a dimension
ds_expanded = dsInterior.swap_dims({'time': 'iter'})

# Interpolate
ds_interpolated = ds_expanded.interp(iter=myIters)

# The interpolated values
ds_expanded = ds_interpolated.swap_dims({'iter': 'time'})
print('Finished cutting and formatting dataset')

# Make plots
saveFrame4(0, imgPath+'vnc_1yr_time-series_cSound_zoom7days.png' )
for number in range(len(ds_expanded.time)):
    saveFrame4(number, imgPath+'vnc_1yr_time-series_cSound_zoom7days_%10.10i.png'%ds_expanded.iter[number] )
