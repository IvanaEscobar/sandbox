ATN 21.Nov.2016.  Notes on MIT profile package:

Target: store data in a  a structure of the form shown below and write it to a netcdf file.  
We show an example of a data set with N = number of data points, i.e.N  = 19890 profiles.
The data structure is in the file itp.nc.
 
To view an example of the expected final product, the netcdf structure in which prfile data will be delivered, first add in the tool box: 
>>addpath('./MITprof_toolbox/');

Read in the file: 
>> MITprof=MITprof_read('itp.nc');

The structure can be viewed:
>> MITprof
             prof_depth: [85x1 double]
             prof_descr: [19890x75 char]
              prof_date: [19890x1 double]
          prof_YYYYMMDD: [19890x1 double]
            prof_HHMMSS: [19890x1 double]
               prof_lon: [19890x1 double]
               prof_lat: [19890x1 double]
             prof_basin: [19890x1 double]
             prof_point: [19890x1 double]
                 prof_T: [19890x85 double]
           prof_Tweight: [19890x85 double]
            prof_Testim: [19890x85 double]
              prof_Terr: [19890x85 double]
             prof_Tflag: [19890x85 double]
                 prof_S: [19890x85 double]
           prof_Sweight: [19890x85 double]
            prof_Sestim: [19890x85 double]
              prof_Serr: [19890x85 double]
             prof_Sflag: [19890x85 double]
       prof_interp_XC11: [19890x1 double]
       prof_interp_YC11: [19890x1 double]
     prof_interp_XCNINJ: [19890x1 double]
     prof_interp_YCNINJ: [19890x1 double]
          prof_interp_i: [19890x1 double]
          prof_interp_j: [19890x1 double]
        prof_interp_lon: [19890x1 double]
        prof_interp_lat: [19890x1 double]
    prof_interp_weights: [19890x1 double]

Steps to get/read/write this structure:
======================================

0. add in the tool box:
>>addpath('./MITprof_toolbox/');

%----------------------------------

1. Interpolation of the raw data at a particular location [lat,lon] and full depth into the MITprof predefined 85 depth levels. 
The vertical levels to which data are to be interpolated are stored in MITprof.prof_depth. They can be stored with the command:
>> MITprof.prof_depth=[5 15 25 35 45 55 65 75 85 95 105 115 125 135 145 155 165 175 185 200 ...
   220 240 260 280 300 320 340 360 380 400 420 440 460 480 500 550 600 650 ...
   700 750 800 850 900 950 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 ...
   2000 2100 2200 2300 2400 2500 2600 2700 2800 2900 3000 3100 3200 3300 3400 ...
   3500 3600 3700 3800 3900 4000 4100 4200 4300 4400 4500 4600 4700 4800 4900 5000];

The preferred vertical interpolation scheme should be interp1 for continuous (in depth) data, or pick the closest depth if mooring data.

In step 1, the following fields need to be loaded and have the noted units:
MITprof.prof_depth	: defined as above
       .prof_descr	: 1 x 30 character for each profile. The length is fixed to 30 characters. 
 			  (Use info of the profile e.g. label, site, cruisename, etc.)
       .prof_date	: using matlab datenum
       .prof_YYYYMMDD	: date as year, month, date
       .prof_HHMMSS	: time as hours, minutes, seconds
       .prof_lon	: degrees EAST
       .prof_lat	: degrees NORTH
       .prof_T		: Potential temperature [degC]
       .prof_Tflag	: 1 = invalid, 0 = valid
       .prof_S		: Salinity [psu]
       .prof_Sflag	: 1 = invalid, 0 = valid
       .prof_U          : Eastward velocity [m/s]  (if there are data) 
       .prof_Uflag	: 1 = invalid, 0 = valid
       .prof_V		: Northward velocity [m/s]
       .prof_Vflag    	: 1 = invalid, 0 = valid

>> MITprof. prof_descr(1,:)
ans =
ITP019890sub_itp1final_00001al

>> MITprof.prof_YYYYMMDD(1,:)
ans =
    20050816
etc.

%----------------------------------

2. Read in the model grid and obtain the Delauney Triangulation points 
The script 
step02_grid_delauney_global.m 

outputs 
MITprof.prof_point, 

which has the grid index for each profile (use ind2sub to get the corresponding subindices).  

The script step02_grid_delauney_global.m reads in lon and lat, which have to be provided in the fields MITprof.prof_lon and MITprof.prof_lat for each profile
The GRID files needed are in the directory DATA.
It also provides a plot with the locations of the profiles

%----------------------------------

3. Read in sigma and climatology fields
The fields for the errors (sigma) and climatology for T and S are in ../DATA 

The script step03_read_2D3D_field.m reads in the files above and produces:
MITprof.prof_Tweight
       .prof_Sweight
       .prof_Testim
       .prof_Sestim
       .prof_basin

%----------------------------------

4. Get model grid tiles etc.
step04a_get_tile_point.m
MITprof.prof_interp_XC11
       .prof_interp_YC11
       .prof_interp_XCNINJ
       .prof_interp_YCNINJ
       .prof_interp_i
       .prof_interp_j
       .prof_interp_lon
       .prof_interp_lat
       .prof_interp_weights

%----------------------------------

5. Write out netcdf file
Write out to a test.nc file:

>>step05a_short_write_nc(MITprof,'test.nc');



