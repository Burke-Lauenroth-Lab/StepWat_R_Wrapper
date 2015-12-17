###################################################
### Stepwat Wrapper Weather file                ###
### Author: Trace E Martyn                      ###
### Date: 12.14.2015                            ###
###################################################

####################################################################################
### Run this code just before running the weather assembly code                  ###
### in that way sites and scenarios are assigned in the R workspace              ###
### Below is modified from Daniel Schlaepfer's code in the SOILWAT R Wrapper     ###
####################################################################################

#### libraries ####
# because it is the old weather database, the weather data is also in the Rsoilwat v27 format and you need to load the package
	
#### connect to the SQLite database ####

  db<-dbDriver("SQLite")
	db<-dbConnect(db,"dbWeatherData_BSE898_FutureEcohydrology_Maurer2002_LLNL10GCMs.sqlite")
	con.env <- NULL
  con.env$con<-db

#########################################################################
#### function to access the weather data from the oldSQLite database ####
#########################################################################
	old_dbW_getWeatherData <- function(Site_id=NULL,lat=NULL,long=NULL,Label=NULL,startYear=NULL,endYear=NULL, Scenario="Current") {
		if(is.null(Site_id) && is.null(Label) && is.null(lat) && is.null(long)) {
			stop("No way to locate weather data from input")
		}
	
		useYears<-FALSE
		useStart<-FALSE
		useEnd  <-FALSE
		if(!is.null(startYear) | !is.null(endYear)) {#See if we should narrow the start end year range
			startYear <- as.integer(startYear)
			if(!is.na(startYear)) useStart<-TRUE
			endYear <- as.integer(endYear)
			if(!is.na(endYear)) useEnd<-TRUE
			if(useStart | useEnd) useYears<-TRUE
			if(useStart & useEnd) {
				if(startYear >= endYear | startYear<0 | endYear<0) {
					stop("Wrong start or end year")
				}
			}
		}
		Site_id<-as.integer(Site_id)
		if(length(Site_id) == 0) {
			Site_id <- dbW_getSiteId(lat,long,Label)
		} else {
			if(!dbGetQuery(con.env$con, paste("SELECT COUNT(*) FROM WeatherData WHERE Site_id=",Site_id,";",sep=""))[1,1]) {
				stop("Site_id does not exist.")
			}
		}
		if(!is.null(Site_id) && is.integer(Site_id) && Site_id >= 0) {
			Scenario <- dbGetQuery(con.env$con, paste("SELECT id FROM Scenarios WHERE Scenario='",Scenario,"';",sep=""))[1,1]
			result <- dbGetQuery(con.env$con, paste("SELECT data FROM WeatherData WHERE Site_id=",Site_id, " AND Scenario=",Scenario,";",sep=""))[[1]][[1]];
			data <- unserialize(memDecompress(result,type="gzip"))
			if(inherits(data, "try-error")) stop(paste("Weather data for Site_id", Site_id, "is corrupted"))
		} else {
			stop(paste("Site_id for", Label, "not obtained."))
		}
	
		if(useYears) {
			if(useStart && useEnd) {
				# adjusting so we actually explore the values of the "year" slots of our soilwatDB object list
				# startYear_idx <- match(startYear,as.integer(names(data)))
				startYear_idx <- match(startYear, 
									   as.integer(unlist(lapply(data, FUN=slot, "year"))))
				# endYear_idx <- match(endYear,as.integer(names(data)))
				endYear_idx <- match(endYear, 
							 as.integer(unlist(lapply(data, FUN=slot, "year"))))
				data <- data[startYear_idx:endYear_idx]
			} else if(useStart) {
				#startYear_idx <- match(startYear,as.integer(names(data)))
				startYear_idx <- match(startYear, 
							   as.integer(unlist(lapply(data, FUN=slot, "year"))))
				# data <- data[startYear_idx:length(as.integer(names(data)))]
				data <- data[startYear_idx:length(as.integer(unlist(lapply(data, FUN=slot, "year"))))]
			} else if(useEnd) {
				# endYear_idx <- match(endYear,as.integer(names(data)))
				endYear_idx <- match(endYear,
									 as.integer(unlist(lapply(data, FUN=slot, "year"))))
				data <- data[1:endYear_idx]
			}
		}
		return(data)
	}

#### Extract weather data from SQLite database ####
##------Copied from SoilWat R Wrapper part 4:
	.local <- function(sid){
		i_sw_weatherList <- list()
		for(k in seq_along(climate.conditions))
			i_sw_weatherList[[k]] <- old_dbW_getWeatherData(Site_id=sid, Scenario=climate.conditions[k])
		return(i_sw_weatherList)
	}

#### make a new large list of weather data ####
	sw_weatherList <- NULL
	for(i in seq_along(sites)){
		sw_weatherList[[i]] <- try(.local(sid=sites[i]), silent=TRUE)
	}

#### save the new weather data large list in folder ####
	save(sw_weatherList, file=file.path(dir.prj, "2015_Weather.RData"))
#### this sw_weatherList  is called in the weather assembly code so keep in R workspace

  
#### send email if flagged #############    
  if (email.flag==TRUE) {
    sendmail(email,"Weather Query Done")}
#############################################################################  
  
