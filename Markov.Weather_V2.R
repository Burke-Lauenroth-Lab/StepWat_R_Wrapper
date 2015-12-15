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
            
     else {
             if (DGF$WET[i]==T & DGF$WET[i-1]==T ) {
                     DGF$W_W[i]<-1
                     DGF$W_D[i]<-0}
             else if (DGF$WET[i]==T & DGF$WET[i-1]==F) {
                     DGF$W_D[i]<-1
                     DGF$W_W[i]<-0}  
             else {
                     DGF$W_D[i]<-0
                     DGF$W_W[i]<-0
             }
        }
  df.list[[g]]<-DGF}} #put the new data frame into a list

# THEN calculate PPT avg and mean for each day
    #!!!! in old literature this is the alpha and beta parameters for a gamma distribution
    #!!!!   but the code in Stepwat uses a normal distribution with mean and SD
    #!!!!   and then if a negative number is drawn it is set to "0"
  
DF<-data.frame(DAY=1:365) #create new dataframe of daily values
DF.DAY<-data.frame(DAY=1:365) #created another new dataframe for daily values
for ( i in 1:365) { #for each day
        avg.ppt<-vector() #create these blank vectors
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
                avg.ppt<-append(avg.ppt,ppt) #append them to vectors
                sd.ppt<-append(sd.ppt,ppt)
                max<-df.list[[g]]$Tmax_C[which(df.list[[g]]$DOY==i)]#extract max temp for the day for each year
                maxw<-df.list[[g]]$Tmax_C[which(df.list[[g]]$DOY==1 & df.list[[g]]$WET==TRUE)]# extract max temp for all wet days
                maxd<-df.list[[g]]$Tmax_C[which(df.list[[g]]$DOY==1 & df.list[[g]]$WET==FALSE)]# extract max temp for all dry days
                min<-df.list[[g]]$Tmin_C[which(df.list[[g]]$DOY==i)] #extract min temp for the day for each year
                minw<-df.list[[g]]$Tmin_C[which(df.list[[g]]$DOY==1 & df.list[[g]]$WET==TRUE)]# extract min temp for all wet days
                mind<-df.list[[g]]$Tmin_C[which(df.list[[g]]$DOY==1 & df.list[[g]]$WET==FALSE)]# extract min temp for all dry days
                tmax.c<-append(tmax.c,max) # append to vector
                tmin.c<-append(tmin.c,min) # append to vector
                tmax.c.w<-append(tmax.c.w,maxw) # append to vector
                tmin.c.w<-append(tmin.c.w,minw) # append to vector
                tmax.c.d<-append(tmax.c.d,maxd) # append to vector
                tmin.c.d<-append(tmin.c.d,mind) # append to vector
                wet_wet<-df.list[[g]]$W_W[which(df.list[[g]]$DOY==i)] #extract if was a wet given wet day
                wet_dry<-df.list[[g]]$W_D[which(df.list[[g]]$DOY==i)] #extract if was a wet given dry day
                prob.wet_wet<-append(prob.wet_wet,wet_wet) #append to vectors
                prob.wet_dry<-append(prob.wet_dry,wet_dry)
                tmax.c<-append(tmax.c,max) # append to vector
                tmin.c<-append(tmin.c,min)
                tmax.c.w<-append(tmax.c.w,maxw) # append to vector
                tmin.c.w<-append(tmin.c.w,minw) # append to vector
                tmax.c.d<-append(tmax.c.d,maxd) # append to vector
                tmin.c.d<-append(tmin.c.d,mind) # append to vector
                
                
                }
        #Calculate correction factors
        #convert to kelvin
        tmax.c<-tmax.c + 273.15 
        tmin.c<-tmin.c + 273.15
        tmax.c.w<-tmax.c.w + 273.15
        tmin.c.w<-tmin.c.w + 273.15
        tmax.c.d<-tmax.c.d + 273.15
        tmin.c.d<-tmin.c.d + 273.15
        
         CF.max.w<-(abs(mean(tmax.c.w)/mean(tmax.c))) + (mean(tmax.c.w)-mean(tmax.c))/mean(tmax.c)
         if (CF.max.w > 1.0) {CF.max.w<-1}
         CF.max.d<-(abs(mean(tmax.c.d)/mean(tmax.c))) + (mean(tmax.c.d)-mean(tmax.c))/mean(tmax.c)
         if (CF.max.d < 1.0) {CF.max.d<-1}
         CF.min.w<-(abs(mean(tmin.c.w)/mean(tmin.c))) + (mean(tmin.c.w)-mean(tmin.c))/mean(tmin.c)
         if (CF.min.w > 1.0) {CF.min.w<-1}
         CF.min.d<-(abs(mean(tmin.c.d)/mean(tmin.c))) + (mean(tmin.c.d)-mean(tmin.c))/mean(tmin.c)
         if (CF.min.d < 1.0) {CF.min.d<-1}
        
        
          DF.DAY$W_W[i]<-sum(prob.wet_wet) #sum all of the wet given wet days for the day across all the years
          DF.DAY$W_D[i]<-sum(prob.wet_dry) #sum all of the wet given dry days for the day across all the years
          DF$avg.ppt[i]<-mean(avg.ppt) #average the ppt across all the years for that day
          DF$sd.ppt[i]<-(sd(sd.ppt)*2)
          DF$CF.max.w[i]<-CF.max.w
          DF$CF.max.d[i]<-CF.max.d
          DF$CF.min.w[i]<-CF.min.w
          DF$CF.min.d[i]<-CF.min.d
          
          } #sd of ppt across all the years X 2 to present a wide range of potential ppts

#THEN calcualte probabilities for each day!
#below is how the old executable calculated probabilities (potential place to revise in the future)


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
  }}
    # print out the probability file that will be read by stepwat
    colnames(DF)<-c("#DOY","PPT_avg", "PPT_sd", "CF.max.w","CF.max.d","CF.min.w","CF.min.d","p[W|W]","p[W|D]")# relabel the columns names 
    DF<-DF[,c("#DOY","p[W|W]","p[W|D]","PPT_avg","PPT_sd","CF.max.w","CF.max.d","CF.min.w","CF.min.d")] #put columns in correct order for output
    rownames(DF)<- NULL #make rownames null (need this or else will have an extra column)
    write.table(format(DF, digits=5), file=paste("mkv_prob.in"), sep="\t", row.names=F,quote=F) #write your year file in your directory
###########################################################################


################## Write mkv_covar.in FILE  ##############################
###################### TEMP and TEMP COVAR MATRIX ########################

df.list<-list() # make list

#remake the list for each year
    
    for (g in 1:yr) {
      DGF<-data.frame(sw_weatherList[[s]][[h]][[g]]@data)
        df.list[[g]]<-DGF}
#THEN extract max and min temp for each day across all years    
    
max.list<-list()
min.list<-list()
for ( i in 1:365) { # for each day
  tmax.c<-vector()
  #tmax.c.w<-vector()
  #tmax.c.d<-vector()
  tmin.c<-vector()
  #tmin.c.w<-vector()
  #tmin.c.d<-vector()
  for (g in 1:yr) { # for each year
    max<-df.list[[g]]$Tmax_C[which(df.list[[g]]$DOY==i)]#extract max temp for the day for each year
    #maxW<-df.list[[g]]$Tmax_C[which(df.list[[g]]$DOY==1 & df.list[[g]]$WET==TRUE)]# extract max temp for all wet days
    #maxd<-df.list[[g]]$Tmax_C[which(df.list[[g]]$DOY==1 & df.list[[g]]$WET==FALSE)]# extract max temp for all dry days
    min<-df.list[[g]]$Tmin_C[which(df.list[[g]]$DOY==i)] #extract min temp for the day for each year
    #minW<-df.list[[g]]$Tmin_C[which(df.list[[g]]$DOY==1 & df.list[[g]]$WET==TRUE)]# extract min temp for all wet days
    #mind<-df.list[[g]]$Tmin_C[which(df.list[[g]]$DOY==1 & df.list[[g]]$WET==FALSE)]# extract min temp for all dry days
    tmax.c<-append(tmax.c,max) # append to vector
    tmin.c<-append(tmin.c,min) # append to vector
    #tmax.c.w<-append(tmax.c.w,maxw) # append to vector
    #tmin.c.w<-append(tmin.c.w,minw) # append to vector
    #tmax.c.d<-append(tmax.c.d,maxd) # append to vector
    #tmin.c.d<-append(tmin.c.d,mind) # append to vector
  }
  min.list[[i]]<-tmin.c # append vector to list
  max.list[[i]]<-tmax.c # append vector to list
   }

#THEN calulate values on a weekly time step
# covar file is in weekly time step... 

week<-seq(7,371,by=7) #list of last day of weeks 
extra.days<-c(363,364,365,1,2,3,4) # these are days used to estiamte week 53
week.df<-data.frame(WEEK=seq(1,53)) # create dataframe
for (w in 1:length(week)) { # for each week
  wg<-week[w]
  min<-vector()
  max<-vector()
  if (w==53) { #special case week 53
    for (d in extra.days) {
        min.day<-(min.list[[d]])
        max.day<-(max.list[[d]])
        min<-append(min,min.day)
        max<-append(max,max.day)
      }
    } else {
    for (i in 1:6) { #to count out days of week
      min.day<-(min.list[[wg-i]]) #extract min temp for each day of week 
      max.day<-(max.list[[wg-i]]) #extract max temp for each day of week 
      min<-append(min,min.day) #append temp to vector
      max<-append(max,max.day) #append temp to vector
    }}
    MIN.MAX<-cov(min,max) #covariance between min and max temp over all days in week for all years
    MIN.MIN<-cov(min,min) #covariance between min temps over all days in week for all years
    MAX.MAX<-cov(max,max) #covariance between max temps over all days in week for all years
    week.df$MAX[w]<-mean(max) #mean max temp for the week 
    week.df$MIN[w]<-mean(min) #mean min temp for the week
    week.df$MIN.MIN[w]<-MIN.MIN #place the covariance numbers in dataframe
    week.df$MAX.MIN[w]<-MIN.MAX
    week.df$MIN.MAX[w]<-MIN.MAX
    week.df$MAX.MAX[w]<-MAX.MAX
}

#THEN write the files
#rename columns
colnames(week.df)<-c("#WEEK","T.MAX.C", "T.MIN.C","cov[MIN.MIN]","cov[MAX.MIN]","cov[MIN.MAX]","cov[MAX.MAX]")# relabel the columns names 
rownames(week.df)<- NULL #make rownames null (need this or else will have an extra column)
write.table(format(week.df, digits=5), file=paste("mkv_covar.in"), sep="\t", row.names=F,quote=F) #write your year file in your directory

#reset directory to site level
setwd(paste(weather.dir,"Site","_",site,sep=""))}

#reset directory to project level
setwd(weather.dir)}

####################################################################################







