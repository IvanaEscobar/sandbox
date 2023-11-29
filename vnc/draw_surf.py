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

def saveFrame(tt, ds, cmap, fname=''):
    if ds.attrs['standard_name']=='ihop_ssp':
        vmin=1403; vmax=1548
    elif ds.attrs['standard_name']=='THETA':
        vmin=0;vmax=30
    elif ds.attrs['standard_name']=='SALT':
        vmin=18;vmax=35.6

    # plot on a rotated llc face/tile/facet
    p = ds.isel(k=0,time=tt).plot(x='j',y='i', cmap=cmap, 
            vmin=vmin, vmax=vmax, extend='neither')
    # flip y-axis
    plt.gca().invert_yaxis()

    plt.grid(alpha=0.3)
        
    ## points of the ihop domain
    lineID = [[128, 139],[148,138]] # in [J],[I] for ds.sel
    plt.plot(lineID[0], lineID[1], color='blue', zorder=3)
    plt.scatter(lineID[0][ 0], lineID[1][ 0], s=20, color='C0', 
                zorder=4, label='source')
    plt.scatter(lineID[0][-1], lineID[1][-1], s=20, color='C1', 
                zorder=4, label='receiver')

    # set tick marks on rotated curvilinear facet *ugly cry*
    ax = p.axes
    xticks = ax.get_xticks()
    yticks = ax.get_yticks()

    ax.set_xticks(xticks)
    ax.set_yticks(yticks)
    ax.set_xlim(ds.j.min(), ds.j.max())
    ax.set_ylim(ds.i.max(), ds.i.min())

    xind = get_da_indices(lon_ticks.j, xticks)
    yind = get_da_indices(lat_ticks.i, yticks)

    ax.set_xticklabels(lon_ticks[xind].data.astype(int))
    ax.set_yticklabels(lat_ticks[yind].data.astype(int))

    # Set labels
    ax.set_xlabel("longitude [$^\circ$E]")
    ax.set_ylabel("latitude [$^\circ$N]")
    ax.set_title("Surface "+ ds.attrs['long_name'] + " at "+ str(ds.dates.isel(time=tt).data) + " UTC")
    plt.legend(loc='lower left')

    if fname:
        plt.savefig(fname, bbox_inches="tight", transparent=True, dpi=300)
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
vnc=ds.isel(i=slice(10,237), i_g=slice(10,238),
            j=slice(12,198), j_g=slice(12,199),
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
print('Start plotting')

# Make plots
for number in range(len(vnc.iter)):
#   fileName = imgPath+'vnc_1yr_surface-cSound_%10.10i.png'%vnc.iter[number]
#   cmap=cm.speed
#   saveFrame(number, vnc.ihop_ssp, cmap, fileName)

#   fileName = imgPath+'vnc_1yr_surface-THETA_%10.10i.png'%vnc.iter[number]
#   cmap=cm.thermal
#   saveFrame(number, vnc.THETA, cmap, fileName)

    fileName = imgPath+'vnc_1yr_surface-SALT_%10.10i.png'%vnc.iter[number]
    cmap=cm.haline
    saveFrame(number, vnc.SALT, cmap, fileName)
