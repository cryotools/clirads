# README #

QUICKSTART

Extract temperature from weather station data set.
Staion T data should be in csv format:
Datetime, value
YYYY-MM-DD hh:mm:ss,float, e.g.
2014-09-29 08:34:00,20.14

Download ERA Interim data:
* temperature 2m
* total cloud cover
* water vapour
* wind speed
* wind direction
* geopotential height
* mean sea level pressure 

Convert from GRIB to netCDF format.

Edit config template and save as a new file.

Edit T_downscaling.R and provide the path and filename of your config file.

Run the code.

### TODO ###
Improvements
* Uncertainty estimation
* Improve handling of time difference
* Flexible handling of time steps (time step in h as config parameter)
* Status bars for file imports
* Check for and remove redundant variables created during runtime 

Development goals
* Precipitation downscaling

### What is this repository for? ###

* Quick summary
* Version


### How do I get set up? ###

* Summary of set up
* Configuration
* Dependencies
* Database configuration
* How to run tests
* Deployment instructions

### Contribution guidelines ###

* Writing tests
* Code review
* Other guidelines

### Who do I talk to? ###

* Repo owner or admin
* Other community or team contact