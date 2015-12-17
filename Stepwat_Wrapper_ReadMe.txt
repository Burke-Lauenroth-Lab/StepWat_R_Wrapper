Stepwat Wrapper ReadMe
Author: Trace Martyn
Date: December 14, 2015

##################################
Files that makeup the Wrapper:
 - Weather.Assembly.Choices_V2.R
 - STEPWAT.Wrapper.MAIN_V3.R
 - StepWat.Wrapper.Code_V3.R
 - RSoilWat.Weather.Data.Query_V2.R

Additionally:
# database containing historical and future weather data for 898 sites across the US West
 - dbWeatherData_BSE898_FutureEcohydrology_Maurer2002_LLNL10GCMs.sqlite
# CSV that lists the ID for the 898 sites across the US West based of lat.long coordinates. Need to use the site IDs to query the above database for weather.
### The site IDs will be used to run the weather query code but the order of the input vector will be used to execute the rest of the code.
###	For example: VECTOR<-c(45,67,101,34) (This vector will be used to query the SQLite database but for the rest of the wrapper the placement in the vector becomes the site name (i.e. 45->1, 67->2, 101->3, 34->4)).
 - SWRuns_InputMaster_BSE898_FutureEcohydrology_v11.csv
# an example weather dataset based on T.MartynÕs 10 Montana sites (can use to test weather assembly and model). Double click to load into R workspace.
 - 2015_Weather.RData
# after the model has run the R script pulls all the output into one large database and sorts the output by GCM or Site to understand GCM and Site variability in the output.
 - CSV.read_SORTING.STEPWAT.R
###################################

###################################
Before running the wrapper the user must have the following folders/files in place (see Example_File_System):

1) Folders for each site labeled 1 through 'N' number of sites. Each folder must include an executable version of Stepwat pulled from GitHub or existing locally (see below for notes on how to install and build from github). The folders must be built previously via the terminal. Folders must be named ÒStepwat.Site.XÓ where X is the number of the site starting at 1 and ending with the maximum N number of sites wanted to run.

2) IF RUNNING MULTIPLE DISTURBANCE REGIMES: The user needs to create a folder called ÔSTEPWAT_DISTÕ in the working directory. This folder contains copies of the Ôrgroup.inÕ file with various combinations of disturbance frequency and grazing frequency. The files must be named Ôrgroup.freqX.grazY.inÕ where X is the number of years for the disturbance (e.g. fire) frequency and Y is the grazing interval.  If not wanting to use grazing then the value of Y is 0 and if not wanting to implement a disturbance (but keep grazing turned on) then the value of X is 0. There is currently no way to implement different grazing intensities but could easily coded into the wrapper. These files are for all sites - you must revise the wrapper to allow these disturbances to change for specific sites.

3) The SQLite database that included the 898 sites used in many Soilwat manuscripts (dbWeatherData_BSE898_FutureEcohydrology_Maurer2002_LLNL10GCMs.sqlite).

####################################

####################################
To run the the Stepwat wrapper the user only needs to open and execute the STEPWAT.Wrapper.MAIN_V3.R file. However if may be useful to open the other files to understand their function in the wrapper.

LIBRARIES:
# the following libraries and used to run the wrapper

REQUIRED LIBRARIES:
RSQLite -  a dependancy for the Rsoilwat31 library.
DBI -  a dependancy for the Rsoilwat31 library.

RECOMMENDED (OPTIONAL) LIBRARIES:
mail - will allow the user to receive email updates about the wrapperÕs progression.
snow - creates a virtual cluster on the computer and may speed up the wrapper runs.
parallel - assists with cluster implementation.

####################################

####################################
WRAPPER FLOW:
1- Query the data from the SQLite database into a local large list in R (uses parts of the RSoilwat Wrapper by D.R. Schlaepfer).

2- Use that large list of weather in R to create individual year weather files or 2 files used for Markov weather generator already written into the C code.

3- Implement the Stepwat code and rename output files.

AFTER THE WRAPPER HAS RUN:
1- Open, save, and close all output csvÕs in Excel (as of 12.14.2015 a quirk that has not been resolved).

2- Run the reading code to create a large database and to create a database sorted by GCM or sorted by site.

NOTE: You can implement only one part of the wrapper but some of the parameters are dependent on above input parameters. Therefore you need to run the Rcode that reads the parameters into R but not ÔsourceÕ the files you do not need.
####################################

####################################
There are currently 7 available ways to assemble weather:
* 1 year interval - basic: resampling one year intervals until desired number of years is created.
* 5 year interval - basic: resampling five year intervals until desired number of years is created.
* 5 year interval - back: resampling one year intervals until desired number of years is created, but not allowing the identified ÔdroughtÕ interval out of the original historical data to be back-to-back.
* 5 year interval - drought: resampling one year intervals until desired number of years is created, but not allowing the identified ÔdroughtÕ interval out of the original historical data to be back-to-back and not have the ÔdroughtÕ interval more than once in 20 years.
* 10 year interval - basic: resampling ten year intervals until desired number of years is created.
* 30 year interval - basic: resampling thirty year intervals until desired number of years is created.
* Markov - uses historical weather data to create Ômkv_prob.inÕ and Ômkv_covar.in files used in the built-in (into the C code) Markov weather generator.
####################################

###################################
Install Stepwat from github:
# change working directory to Desktop
> cd Desktop
# clone Stepwat from github.com to a folder called Stepwat on the Desktop
> git clone https://github.com/Burke-Lauenroth-Lab/StepWat.git --branch SoilWat31_drs --single-branch Stepwat 
# change directory into new folder
> cd Stepwat
# update the submodule
> git submodule update Ñ-init Ñ-recursive
# build Stepwat
> make
###################################

