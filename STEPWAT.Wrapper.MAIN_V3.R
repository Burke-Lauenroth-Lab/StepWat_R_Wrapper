###################################################
### Stepwat Wrapper MAIN file                   ###
### Author: Trace E Martyn                      ###  
###################################################
### V1 9.28.2015                                ###
### V2 added Markov weather file code 11.1.2015 ###
### V3 added dist and grazing code 11.19.2015   ###
### V4 updated code and notes 12.14.2015        ###
###################################################

##### packages #####################################################################
library(mail)
library(snow)
library(parallel)
library(DBI)
library(RSQLite)
library(Rsoilwat31)

#### establish cluster #############################################################
cl<-makeCluster((detectCores()/2))

#### input email if want emails when code is done running (see email.flag below)####
email<-"tmartyn@uwyo.edu"

#### source directory and files ####################################################
source.dir<-"/Users/tmartyn/Dropbox2/Dropbox/RCODE/StepWat.Wrapper.TM_V4/"
setwd(source.dir)
query.file<-paste(source.dir,"RSoilWat31.Weather.Data.Query_V2.R", sep="")
assemble.file<-paste(source.dir,"Weather.Assembly.Choices_V2.R", sep="")
wrapper.file<-paste(source.dir,"StepWat.Wrapper.Code_V3.R", sep="")
markov.file<-paste0(source.dir,"Markov.Weather_V2.R")



######## Weather Query Code ##########################################################

########### Set parameters ##########################################
#send email when weather query is done
#weather query usually doesn't take more than 3 minutes
email.flag<-FALSE

#### Set directories and SQLite database file name
dir.prj <- "/Users/tmartyn/Dropbox2/Dropbox/RCODE/StepWat.Wrapper.TM_V4/"
dbWeatherDataFile <- file.path(dir.prj, "dbWeatherData_BSE898_FutureEcohydrology_Maurer2002_LLNL10GCMs.sqlite")

#### Setup for the weather aquisition (years, scenarios, timeperiod, GCMs, etc.) 
simstartyr <- 1979
endyr <- 2010
climate.ambient <- "Current"
climate.conditions <- c(climate.ambient,  "RCP45.ACCESS1-0", "RCP45.CanESM2", "RCP45.CESM1-CAM5", "RCP45.CMCC-CM", "RCP45.CNRM-CM5", "RCP45.CSIRO-Mk3-6-0", "RCP45.HadGEM2-ES", "RCP45.IPSL-CM5A-MR", "RCP45.MIROC5", "RCP45.NorESM1-M",
                        "RCP85.ACCESS1-0", "RCP85.CanESM2", "RCP85.CESM1-CAM5", "RCP85.CMCC-CM", "RCP85.CNRM-CM5", "RCP85.CSIRO-Mk3-6-0", "RCP85.HadGEM2-ES", "RCP85.IPSL-CM5A-MR", "RCP85.MIROC5", "RCP85.NorESM1-M")

# if you want 2030-2060 use 50; if you want 2070-2100 use 90 below
deltaFutureToSimStart_yr <- c(50,90)
# this does not change
downscaling.method <- c("hybrid-delta")

#This is from Daniel's code
temp <- climate.conditions[!grepl(climate.ambient, climate.conditions)] #make sure 'climate.ambient' is first entry
if(length(temp) > 0){
  temp <- paste0(deltaFutureToSimStart_yr, "years.", rep(temp, each=length(deltaFutureToSimStart_yr)))	#add (multiple) deltaFutureToSimStart_yr
  temp <- paste0(downscaling.method, ".", rep(temp, each=length(downscaling.method))) #add (multiple) downscaling.method
}

# make a list of all future and current scenarios putting "Current" first	
climate.conditions <-  c("Current",temp)
temp<-climate.conditions

#these are all of the sites I want to access data from
#site numbers are from SWRuns_Inputraster_BSE898_FutreEcohydrology_v11.csv
#will be coded in numberical order: 45==1,68==2,etc.
sites<-c(45,68,101,70,105,144,155,106,43,51) 


##################
source(query.file)
##################

############################### End Weather Query Code ################################



######## Weather assembly Code ########################################################

########### Set parameters ##########################################
#send email when weather assembly is done
#weather assembly takes about 20 minutes for 10 sites
email.flag<-FALSE

# set where you would like the weather files outputted
weather.dir<-"/Users/tmartyn/Desktop/StepWat.Wrapper/StepWat.Weather.Markov.Test/"
dir.create(weather.dir, showWarnings = FALSE) 

# number of sites run
S<-length(sites)

# number of GCM X RCP X Periods run (number of scenarios run in the query code)
H<-length(temp)


#@@@@@@@@@@@@@@@##### For weather interval assemblies #######@@@@@@@@@@@@#
# number of years in one section 
K<-30
# interval size
INT<-30
# final number of years wanted
FIN<-30
# resampling time
RE<-FIN/INT

#### type ##########################################
# choose between "basic" (for 1,5,10,30 year); "back" (for 5 year non-driest back-to-back);
#         OR "drought" (for 5 year non-driest back-to-back and only once in 20 years); or "markov"
#         (for markov code output) !!!! if using Markov remember to flag it in weathersetup.in !!!!
   TYPE<-"basic"
####################################################

# if assembling weather data this sources that file
#####################
source(assemble.file)
#####################
#@@@@@@@@###################################################@@@@@@@@@@@@#


##@@@@@@@@@@@@@@################################################@@@@@@@@@@@@@@@##
###@@@@@@@@@@@@###### MARKOV Weather Generator Code #############@@@@@@@@@@@@@###

# if using the Markov weather generator run the following code instead
yr<-30 # number of years to use to create markov file CANNOT be larger than available years in
       #      the sqlite database!!!!!!!

TYPE<-"markov"

# if using markov weather generator below sources the file that creates the C code inputs
#####################
source(markov.file)
#####################


############# End Weather Assembly Code#########################################################








############### Run Wrapper Code ############################################################

########### Set parameters ###############################################
#send email when each site done and all runs done
email.flag<-TRUE

# where are the individual site stpewat folders
# Stepwat Folders should have the format "Stepwat.Site.1" for each site 1 through N
directory<-"/Users/tmartyn/Desktop/"
#s - select which site to run either all (say 1-10 as shown below) or only a couple '<-c(1,5)'
sites<-c(1,2,3,4,5,6,7,8,9,10) 
#g
GCM<-c("Current","CSIRO-Mk3-6-0","HadGEM2-ES","IPSL-CM5A-MR","MIROC5","NorESM1-M","ACCESS1-0","CanESM2","CESM1-CAM5","CMCC-CM","CNRM-CM5")
#r
RCP<-c("RCP45","RCP85")
#y
YEARS<-c("50years","90years")
# Disturbance Flag
dist.graz.flag<-F
# disturbance folder
dist.directory<-"/Users/tmartyn/Desktop/STEPWAT_DIST/"
#distubrance freqency 
dist.freq<-c(50,2,0) # if not using disturbance but are using grazing set 'dist.freq<-0'
#grazing freq
graz.freq<-c(1,0) # if only using distrubance and not grazing set 'graz.freq<-0'


####################
source(wrapper.file)
####################

################ End Wrapper Code ########################################################



stopCluster(cl)
