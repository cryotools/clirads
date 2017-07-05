# Configuration file for the temperature downscaling script
# Edit according to your local setup, the safe (an rename for each study area / dataset).
# Finally, change first row in the main file to load configuration form this file.
#
# See README for further information.

###############################
# General setup               #
###############################

# Work directory (local HD recommended for performance)
wd_PATH <- "/path/to/processing-directory"
  
###############################
# Weather station information #
###############################

name_station <- "Name of the weather station"  # For labeling only
time_difference <- "+ 6h"                      # Difference to UTC (h)

# Coordinates of the respective field station 
# (lon/lat, decimal degrees)
x_coordinate <- 12.34    
y_coordinate <- 56.78

# Station data path and filenames
station_PATH <- "/path/to/station_data"         # No tailing '/'
station_data_file <- "your_station_data" 

# Staion T data should be in csv format:
# Datetime, value
# YYYY-MM-DD hh:mm:ss,float, e.g.
# 2014-09-29 08:34:00,20.14


###############################
# ERA data configuration      #
###############################

# ERA data path and filenames
ERA_PATH <- "/path/to/ERA_data"                 # No tailing '/'

# ERA data should provided in netCDF format
# Convert using grib2netcdf
ERA_T2m_file <- "temperature_2m.nc"
ERA_GPH_file <- "geopotential_height.nc"
ERA_TCWV_file <- "tc_water_vapour.nc"
ERA_UCW_file <- "wind_u-component.nc"
ERA_VCW_file <- "wind_v-component.nc"
ERA_MSLP_file <- "mean_sea_level_pressure.nc"
ERA_TCC_file <- "total_cloud_cover.nc"




