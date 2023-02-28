import requests

def load_data():
    #NetCDF files of OOI Pioneer Profiler Moorings: Temperature, Salinity, Pressure, Density
    urls = [
    'https://erddap.dataexplorer.oceanobservatories.org/erddap/tabledap/ooi-cp01cnpm-wfp01-03-ctdpfk000.nc?time%2Csea_water_practical_salinity_profiler_depth_enabled%2Csea_water_density_profiler_depth_enabled%2Csea_water_pressure_profiler_depth_enabled%2Csea_water_temperature_profiler_depth_enabled%2Csea_water_practical_salinity_profiler_depth_enabled_qc_agg%2Csea_water_density_profiler_depth_enabled_qc_agg%2Csea_water_pressure_profiler_depth_enabled_qc_agg%2Csea_water_temperature_profiler_depth_enabled_qc_agg%2Cz',
    'https://erddap.dataexplorer.oceanobservatories.org/erddap/tabledap/ooi-cp02pmci-wfp01-03-ctdpfk000.nc?time%2Csea_water_practical_salinity_profiler_depth_enabled%2Csea_water_density_profiler_depth_enabled%2Csea_water_pressure_profiler_depth_enabled%2Csea_water_temperature_profiler_depth_enabled%2Csea_water_practical_salinity_profiler_depth_enabled_qc_agg%2Csea_water_density_profiler_depth_enabled_qc_agg%2Csea_water_pressure_profiler_depth_enabled_qc_agg%2Csea_water_temperature_profiler_depth_enabled_qc_agg%2Cz',
    'https://erddap.dataexplorer.oceanobservatories.org/erddap/tabledap/ooi-cp02pmco-wfp01-03-ctdpfk000.nc?time%2Csea_water_practical_salinity_profiler_depth_enabled%2Csea_water_density_profiler_depth_enabled%2Csea_water_pressure_profiler_depth_enabled%2Csea_water_temperature_profiler_depth_enabled%2Csea_water_practical_salinity_profiler_depth_enabled_qc_agg%2Csea_water_density_profiler_depth_enabled_qc_agg%2Csea_water_pressure_profiler_depth_enabled_qc_agg%2Csea_water_temperature_profiler_depth_enabled_qc_agg%2Cz',
    'https://erddap.dataexplorer.oceanobservatories.org/erddap/tabledap/ooi-cp02pmui-wfp01-03-ctdpfk000.nc?time%2Csea_water_practical_salinity_profiler_depth_enabled%2Csea_water_density_profiler_depth_enabled%2Csea_water_pressure_profiler_depth_enabled%2Csea_water_temperature_profiler_depth_enabled%2Csea_water_practical_salinity_profiler_depth_enabled_qc_agg%2Csea_water_density_profiler_depth_enabled_qc_agg%2Csea_water_pressure_profiler_depth_enabled_qc_agg%2Csea_water_temperature_profiler_depth_enabled_qc_agg%2Cz',
    'https://erddap.dataexplorer.oceanobservatories.org/erddap/tabledap/ooi-cp02pmuo-wfp01-03-ctdpfk000.nc?time%2Csea_water_practical_salinity_profiler_depth_enabled%2Csea_water_density_profiler_depth_enabled%2Csea_water_pressure_profiler_depth_enabled%2Csea_water_temperature_profiler_depth_enabled%2Csea_water_practical_salinity_profiler_depth_enabled_qc_agg%2Csea_water_density_profiler_depth_enabled_qc_agg%2Csea_water_pressure_profiler_depth_enabled_qc_agg%2Csea_water_temperature_profiler_depth_enabled_qc_agg%2Cz',
    'https://erddap.dataexplorer.oceanobservatories.org/erddap/tabledap/ooi-cp03ispm-wfp01-03-ctdpfk000.nc?time%2Csea_water_practical_salinity_profiler_depth_enabled%2Csea_water_density_profiler_depth_enabled%2Csea_water_pressure_profiler_depth_enabled%2Csea_water_temperature_profiler_depth_enabled%2Csea_water_practical_salinity_profiler_depth_enabled_qc_agg%2Csea_water_density_profiler_depth_enabled_qc_agg%2Csea_water_pressure_profiler_depth_enabled_qc_agg%2Csea_water_temperature_profiler_depth_enabled_qc_agg%2Cz',
    'https://erddap.dataexplorer.oceanobservatories.org/erddap/tabledap/ooi-cp04ospm-wfp01-03-ctdpfk000.nc?time%2Csea_water_practical_salinity_profiler_depth_enabled%2Csea_water_density_profiler_depth_enabled%2Csea_water_pressure_profiler_depth_enabled%2Csea_water_temperature_profiler_depth_enabled%2Csea_water_practical_salinity_profiler_depth_enabled_qc_agg%2Csea_water_density_profiler_depth_enabled_qc_agg%2Csea_water_pressure_profiler_depth_enabled_qc_agg%2Csea_water_temperature_profiler_depth_enabled_qc_agg%2Cz'
    ]
    
    fnames = [  'cp01cnpm',
                'cp02pmci',
                'cp02pmco',
                'cp02pmui',
                'cp02pmuo',
                'cp03ispm',
                'cp04ospm'    ]
    dataPath = '/scratch2/ivana/data/ooi-pioneer/'
    
    for url, fname in zip(urls, fnames):
        r = requests.get(url, allow_redirects=True)
        open( (dataPath + fname + '.nc'), 'wb').write(r.content)

    return print ("Loaded data in:\n%s" % dataPath)
