###################################################
### Stepwat Wrapper Markov file                 ###
### Author: Trace E Martyn                      ###
### Date: 12.14.2015                            ###
###################################################
#################################################
### To create Markov weather input files    #####
###     mkv_prob.in & mkv_covar.in          #####
#################################################

setwd(weather.dir)

#### Take the large list, resample, and make new data files ####
for (s in 1:S) { # i = number of sites
  site<-s
  dir.create(paste0("Site","_",site), showWarnings = FALSE) #create directory for files
  setwd(paste("Site","_",site,sep="")) # move to directory just created
  for (h in 1:H) { #h = number of GCM X RCP X Times (usually 40 if using GCMs, 1 if using current/historical)
    scen<-temp[h]
    #yr<-length(sw_weatherList[[s]][[h]])#pull out which scenario
    dir.create(paste0("Site","_",site,"_",scen), showWarnings = FALSE) #create a new directory with the site number and scenario name 
    setwd(paste("Site","_",site,"_",scen,sep="")) #reset the working directory into that new directory
    list<-list() #make blank lists
    data1L<-sw_weatherList[[s]][[h]] #pull out the list for the site, GCM, and year
    samplelist<-c(data1L)
    
    
    
    ########### make weather files ##################
    #year<-1901 #reset year to start at 1901 for everything - makes runnig STEPWAT much easier (no changing input files...)
    #for (t in 1:length(samplelist)) { #t = number of resampled lists (ie 100 years -> t=10 300 years -> t=30)
    #  x<-data.frame(samplelist[[t]]@data) # create a dataframe of the days within the year
    #  colnames(x)<-c("#DOY","Tmax_C","Tmin_C","PPT_cm") # relabel the columns names 
    #  rownames(x)<- NULL #make rownames null (need this or else will have an extra column)
    #  write.table(x, file=paste("weath.",year,sep=""), sep="\t", row.names=F,quote=F) #write your year file in your directory
    #  year<-year+1 #increment year
    #}
###########################################################################
    
############ make mkv_prob.in file #############   
############ PRECIP and PRECIP PROBS ###########
df.list<-list() # create blonk list
    
################################################
########### count number of wet days ###########
    
for (g in 1:yr) { # for each year user specifies to use to create markov files
  DGF<-data.frame(sw_weatherList[[s]][[h]][[g]]@data) #extract the weather for each year
  for (i in 1:365) { # for each day of year
    p<-DGF[i,"PPT_cm"] #extract PPT data for the day
    if (p > 0) {
      DGF$WET[i]<-"TRUE" # if PPT >0 the day is labeled wet
    }
    else { DGF$WET[i]<-"FALSE"}} # if not labeled dry

### THEN count the number of W_W and W_D days across all years  
  
  for (i in 1:365) { # for each day
    if (i==1) { #special case for day 1 (look to day 365)
            if (DGF$WET[i]==T & DGF$WET[i+364]==T ) { #if the last day and the current day is wet
                    DGF$W_W[i]<-1 #wet given wet (W_W) is labeled "1"
                    DGF$W_D[i]<-0} #wet given dry (W_D) is labeled "0"
            else if (DGF$WET[i]==T & DGF$WET[i+364]==F) { # if the last day was dry and the current wet
                    DGF$W_D[i]<-1 #wet given dry is labled "1"
                    DGF$W_W[i]<-0}  #wet given wet is labeled "0"
            else {
                    DGF$W_D[i]<-0 #labled "0"
                    DGF$W_W[i]<-0 #labeled"0"
            }}
            
     else { #count number of wet days given previous day was wet (coded as W_W <- 1)
             if (DGF$WET[i]==T & DGF$WET[i-1]==T ) {
                     DGF$W_W[i]<-1
                     DGF$W_D[i]<-0}
       #count number of wet days given previous day was day (coded as W_D <- 1)
             else if (DGF$WET[i]==T & DGF$WET[i-1]==F) {
                     DGF$W_D[i]<-1
                     DGF$W_W[i]<-0}  
       #count number of dry days given previous day was dry (coded as W_W & W_D <- 0)
             else {
                     DGF$W_D[i]<-0
                     DGF$W_W[i]<-0
             }
        }
  df.list[[g]]<-DGF}} #put the new data frame into a list to be called later for further calculations
    
################################################

##################################################
#### calculate PPT avg and mean for each day #####
    #!!!!   in old literature (the original FORTRAN code) this is the alpha and beta parameters for a gamma distribution
    #!!!!   but the code in Stepwat uses a normal distribution with mean and SD
    #!!!!   and then if a negative number is drawn it is set to "0"
  
DF<-data.frame(DAY=1:365) #create new dataframe of daily values (these will ultimately be final values)
    
DF.DAY<-data.frame(DAY=1:365) #created temporary new dataframe for daily values (holds temporary values before use in further calculations)

for ( i in 1:365) { #for each day
  
  #create these blank vectors
  
        avg.ppt<-vector() 
        sd.ppt<-vector() 
        prob.wet_wet<-vector()
        prob.wet_dry<-vector()
        tmax.c<-vector()
        tmax.c.w<-vector()
        tmax.c.d<-vector()
        tmin.c<-vector()
        tmin.c.w<-vector()
        tmin.c.d<-vector()
        
        for (g in 1:yr) { #within each year
          
                ppt<-df.list[[g]]$PPT_cm[which(df.list[[g]]$DOY==i)] #extract daily PPT values
          
            # only if the ppt for that day is > 0 do we add it to the vectors that we will later average and calc SD
                
                if (ppt >0) {
                avg.ppt<-append(avg.ppt,ppt) #append them to vectors
                sd.ppt<-append(sd.ppt,ppt)
                } # close loop @line 120
            
            # extract values from the historical data for temperature values for dry/wet/all days  
              
                ## maximum temperature 
                max<-df.list[[g]]$Tmax_C[which(df.list[[g]]$DOY==i)]#extract max temp for the day for each year
                maxw<-df.list[[g]]$Tmax_C[which(df.list[[g]]$DOY==i & df.list[[g]]$WET==TRUE)]# extract max temp for all wet days
                maxd<-df.list[[g]]$Tmax_C[which(df.list[[g]]$DOY==i & df.list[[g]]$WET==FALSE)]# extract max temp for all dry days
                tmax.c<-append(tmax.c,max) # append to vector
                tmax.c.w<-append(tmax.c.w,maxw) # append to vector
                tmax.c.d<-append(tmax.c.d,maxd) # append to vector
                
                ## minimum temperature 
                min<-df.list[[g]]$Tmin_C[which(df.list[[g]]$DOY==i)] #extract min temp for the day for each year
                minw<-df.list[[g]]$Tmin_C[which(df.list[[g]]$DOY==i & df.list[[g]]$WET==TRUE)]# extract min temp for all wet days
                mind<-df.list[[g]]$Tmin_C[which(df.list[[g]]$DOY==i & df.list[[g]]$WET==FALSE)]# extract min temp for all dry days
                tmin.c<-append(tmin.c,min) # append to vector
                tmin.c.w<-append(tmin.c.w,minw) # append to vector
                tmin.c.d<-append(tmin.c.d,mind) # append to vector
                
                ## make a list of W_W and W_D days
                wet_wet<-df.list[[g]]$W_W[which(df.list[[g]]$DOY==i)] #extract if was a wet given wet day
                wet_dry<-df.list[[g]]$W_D[which(df.list[[g]]$DOY==i)] #extract if was a wet given dry day
                prob.wet_wet<-append(prob.wet_wet,wet_wet) #append to vector
                prob.wet_dry<-append(prob.wet_dry,wet_dry) #append to vector
                } # close loop @line 114
    
      ## Calculate correction factors 
    
                # convert to kelvin so that all values are > 0 (necessary for below calculations)
                tmax.c<-tmax.c + 273.15 
                tmin.c<-tmin.c + 273.15
                tmax.c.w<-tmax.c.w + 273.15
                tmin.c.w<-tmin.c.w + 273.15
                tmax.c.d<-tmax.c.d + 273.15
                tmin.c.d<-tmin.c.d + 273.15
        
                # caluclate temparture correction factors based on if the day was wet or dry
                #   The correct factors will oscilate around 1.0 
                #   With a value of 1.0 = no change in tempeerature
                #        a value > 1.0 = a higher temperature than average
                #        a value < 1.0 = a lower temperature than average
                #   Absolute value is taken to assure values above 0
               
         # max temp modifier on wet day
         CF.max.w<-(abs(mean(tmax.c.w)/mean(tmax.c))) + (mean(tmax.c.w)-mean(tmax.c))/mean(tmax.c)
        
         # max temp modifier on dry day
         CF.max.d<-(abs(mean(tmax.c.d)/mean(tmax.c))) + (mean(tmax.c.d)-mean(tmax.c))/mean(tmax.c)
         
         # min temp modifier on wet day
         CF.min.w<-(abs(mean(tmin.c.w)/mean(tmin.c))) + (mean(tmin.c.w)-mean(tmin.c))/mean(tmin.c)
         
         # min temp modifier on dry day
         CF.min.d<-(abs(mean(tmin.c.d)/mean(tmin.c))) + (mean(tmin.c.d)-mean(tmin.c))/mean(tmin.c)
         
         # capped modifiers so wet days cannot be hotter than average and dry days cooler than average
         if (CF.max.w > 1.0) {CF.max.w<-1}
         if (CF.max.d < 1.0) {CF.max.d<-1}
         if (CF.min.w > 1.0) {CF.min.w<-1}
         if (CF.min.d < 1.0) {CF.min.d<-1}
         
         # these statements are included because there are possibilities that there were not any wet/dry days for a particular DOY
         if (CF.max.w=='NaN'){CF.max.w<-1}
         if (CF.max.d=='NaN'){CF.max.d<-1}
         if (CF.min.w=='NaN'){CF.min.w<-1}
         if (CF.min.d=='NaN'){CF.min.d<-1}
        
         # input sum of wet and dry days into daily dataframe to be used in probability calculations below
          DF.DAY$W_W[i]<-sum(prob.wet_wet) #sum all of the wet given wet days for the day across all the years
          DF.DAY$W_D[i]<-sum(prob.wet_dry) #sum all of the wet given dry days for the day across all the years
          
          # average the ppt for wet days
          DF$avg.ppt[i]<-mean(avg.ppt) #average the ppt across all the years for that day
          
          # calculate ppt SD - if there are less than 2 days the SD is set to 0.0
          if(length(sd.ppt) < 2) {DF$sd.ppt[i]<-0.0}
          else {DF$sd.ppt[i]<-(sd(sd.ppt))}
          
          # input correcitno factors into dataframe
          DF$CF.max.w[i]<-CF.max.w
          DF$CF.max.d[i]<-CF.max.d
          DF$CF.min.w[i]<-CF.min.w
          DF$CF.min.d[i]<-CF.min.d
          
} # close for loop @line 99

        ### THEN calcualte probabilities of precipitation for each day

        for ( i in 1:365) { # for each day
          if (i==1) { # special case for day #1
            DF$W_W[i]<-DF.DAY$W_W[i]/(DF.DAY$W_W[i+364]+DF.DAY$W_D[i+364])
            DF$W_D[i]<-DF.DAY$W_D[i]/(yr-(DF.DAY$W_W[i+364]+DF.DAY$W_D[i+364]))}
          else {
            
            #probability of wet|wet is the number of wet given wet years for that day divided by the number
            #   of total wet days from the previous day
            DF$W_W[i]<-DF.DAY$W_W[i]/(DF.DAY$W_W[i-1]+DF.DAY$W_D[i-1])
            
            #prbability of wet|dry is the number of wet given dry years for that day divdied by the number of
            #   total years (yrs identified by user) minus the total number of wet days from the previous day
            #   or the number of dry days
            DF$W_D[i]<-DF.DAY$W_D[i]/(yr-(DF.DAY$W_W[i-1]+DF.DAY$W_D[i-1]))
          } # end if/else statement @line 215 
          
          } # close for loop @line 212 


#### print out the probability file (mkv_prob.in) that will be read by stepwat
  
      # rename columns
      colnames(DF)<-c("#DOY","PPT_avg", "PPT_sd", "CF.max.w","CF.max.d","CF.min.w","CF.min.d","p[W|W]","p[W|D]")# relabel the columns names 
      
      # extract columns needed for input file  
      DF<-DF[,c("#DOY","p[W|W]","p[W|D]","PPT_avg","PPT_sd","CF.max.w","CF.max.d","CF.min.w","CF.min.d")] #put columns in correct order for output
      
      # make rownames null (need this or else will have an extra column)  
      rownames(DF)<- NULL 
      
      # write out file
      write.table(format(DF, digits=5), file=paste("mkv_prob.in"), sep="\t", row.names=F,quote=F) #write your year file in your directory
        
###########################################################################

##########################################################################
################## Write mkv_covar.in FILE  ##############################
###################### TEMP and TEMP COVAR MATRIX ########################
## values are on a weekly time-step

df.w.list<-list() # make new empty list
      
      #remake the list for each year
      
for (g in 1:yr) { # fill list with historical data
        DGF<-data.frame(sw_weatherList[[s]][[h]][[g]]@data)
        df.w.list[[g]]<-DGF}      

# make empty list for daily max and min temperatures within a week
max.list<-list()
min.list<-list()


for ( i in 1:365) { # for each day
  
  # make empty vectors for max and min temperatures (DOY for entire historical record)
  tmax.c<-vector()
  tmin.c<-vector()

  for (g in 1:yr) { # for each year
    
    max<-df.w.list[[g]]$Tmax_C[which(df.w.list[[g]]$DOY==i)] # extract max temp for the day for each year
    tmax.c<-append(tmax.c,max) # append to daily vector
    
    min<-df.w.list[[g]]$Tmin_C[which(df.w.list[[g]]$DOY==i)] # extract min temp for the day for each year
    tmin.c<-append(tmin.c,min) # append to daily vector
   
    } # close for loop @line 271
  
  # add daily values to weekly list
  min.list[[i]]<-tmin.c # append vector to list
  max.list[[i]]<-tmax.c # append vector to list
   } # close for loop @line 265


### calculate values on a weekly timestep

# create a list of DOY for last day of each week (year starting on day 1 of a week)
week<-seq(7,371,by=7) 

extra.days<-c(363,364,365,1,2,3,4) # these are days used to estiamte week 53

week.df<-data.frame(WEEK=seq(1,53)) # create weekly dataframe (will hold final values)

# for each week calcualte the covariance matrix
for (w in 1:length(week)) { # for each week
  wg<-week[w] # extract the first week
  
  # create empty vectors
  min<-vector()
  max<-vector()
  
  
  if (w==53) { #special case week 53
    for (d in extra.days) {
        # extract daily data for each week 
        min.day<-(min.list[[d]])
        max.day<-(max.list[[d]])
        
        # append daily data to new list
        min<-append(min,min.day)
        max<-append(max,max.day)
      }
    } # end special case for week 53
  
  else {
    
    
    for (i in 1:6) { #to count out days of week
    #DOY = last day for the week (wg) minus 1,2,3,4,5,6 sequentially  
    
      min.day<-(min.list[[wg-i]]) #extract min temp for each day of week 
      min<-append(min,min.day) #append temp to vector
      max.day<-(max.list[[wg-i]]) #extract max temp for each day of week 
      max<-append(max,max.day) #append temp to vector
    } # close for loop @line 320
  } # end if/else statement @line 305

  # calculate covariance between temperatures  
    MIN.MAX<-cov(min,max) #covariance between min and max temp over all days in week for all years
    MIN.MIN<-cov(min,min) #covariance between min temps over all days in week for all years
    MAX.MAX<-cov(max,max) #covariance between max temps over all days in week for all years
    
    # add values into weekly dataframe
    week.df$MAX[w]<-mean(max) #mean max temp for the week 
    week.df$MIN[w]<-mean(min) #mean min temp for the week
    week.df$MIN.MIN[w]<-MIN.MIN #place the covariance numbers in dataframe for MINxMIN
    week.df$MAX.MIN[w]<-MIN.MAX #place the covariance numbers in dataframe for MINxMAX
    week.df$MIN.MAX[w]<-MIN.MAX #the above repeated because both are needed for Stepwat inpur
    week.df$MAX.MAX[w]<-MAX.MAX #place the covariance numbers in dataframe for MAXxMAX
}

#### print out the covariance file (mkv_covar.in) that will be read by stepwat
    
    # rename columns
    colnames(week.df)<-c("#WEEK","T.MAX.C", "T.MIN.C","cov[MIN.MIN]","cov[MAX.MIN]","cov[MIN.MAX]","cov[MAX.MAX]")# relabel the columns names 
    
    # make rownames null (need this or else will have an extra column)
    rownames(week.df)<- NULL
    
    # write out file
    write.table(format(week.df, digits=5), file=paste("mkv_covar.in"), sep="\t", row.names=F,quote=F) #write your year file in your directory

# reset directory to site level
setwd(paste(weather.dir,"Site","_",site,sep=""))}

# reset directory to project level
setwd(weather.dir)}

####################################################################################







