###################################################
### Stepwat Wrapper Weather file                ###
### Author: Trace E Martyn                      ###
### Date: 12.14.2015                            ###
###################################################

#################################################
### Run RSoilWat31.Weather.Data.Query_TM.R
### just before running this code... in that way sites and temp are assigned
#################################################

#################################################
### main code #####
#################################################

############################################################### 
if (INT==5 & TYPE=="basic") {
############################################################### 

  
  
#################################################
########## 5 year resampling interval - basic ###########
#################################################
setwd(weather.dir)
#### Take the large list, resample, and make new data files ####
for (i in 1:S) { # i = number of sites
  site<-i
  dir.create(paste0("Site","_",site), showWarnings = FALSE)
  setwd(paste("Site","_",site,sep=""))
  for (h in 1:H) { #h = number of GCM X RCP X Times (usually 40 if using GCMs, 1 if using current/historical, 41 if using both GCM and Current)
    scen<-temp[h] #pull out which scenario
    dir.create(paste0("Site","_",site,"_",scen), showWarnings = FALSE) #create a new directory with the site number and scenario name 
    setwd(paste("Site","_",site,"_",scen,sep="")) #reset the working directory into that new directory
    list1<-list() #make 6 blank lists
    list2<-list()
    list3<-list()
    list4<-list()
    list5<-list()
    list6<-list()
    for (k in 1:K) { #k=number of years
      data1L<-sw_weatherList[[i]][[h]][[k]] #pull out the list for the site, GCM, and year
      if (k < 6) { # putting the decades into different lists so we can resample them
        list1<-append(list1,data1L)}
      if (k > 25 & k < 31) { 
        list6<-append(list6,data1L)}
      if (k > 5 & k < 11) {
        list2<-append(list2,data1L)}
      if (k > 10 & k < 16) {
        list3<-append(list3,data1L)}
      if (k > 15 & k < 21) {
        list4<-append(list4,data1L)}
      if (k > 20 & k < 26) {
        list5<-append(list5,data1L)}
    }
    largelist<-list(list1,list2,list3,list4,list5,list6) # create a large list of the decadal lists
    samplelist<-sample(largelist,60,replace=T) #resample that large list (10 for 100yrs, 30 for 300yrs)
    year<-1901 #reset year to start at 1901 for everything - makes runnig STEPWAT much easier (no changing input files...)
    for (t in 1:60) { #t = number of resampled lists (ie 100 years -> t=10 300 years -> t=30)
      for (r in 1:5) { # r = number of years in each list1,list2,list3 (ie 10years,5years,2years)
        x<-data.frame(samplelist[[t]][[r]]@data) # create a dataframe of the days within the year
        colnames(x)<-c("#DOY","Tmax_C","Tmin_C","PPT_cm") # relabel the columns names 
        rownames(x)<- NULL #make rownames null (need this or else will have an extra column)
        write.table(x, file=paste("weath.",year,sep=""), sep="\t", row.names=F,quote=F) #write your year file in your directory
        year<-year+1 #increment year
      }
    }
    setwd(paste(weather.dir,"Site","_",site,sep=""))}
  setwd(weather.dir)} # reset initial working directory}

  
  
###############################################################   
} else if (INT==5 & TYPE=="back") {
############################################################### 

  
  
#################################################
########## 5 year resampling interval - drought not back-to-back ###########
#################################################
setwd(weather.dir)
#### Take the large list, resample, and make new data files ####
for (i in 1:S) { # i = number of sites
  site<-i
  dir.create(paste0("Site","_",site), showWarnings = FALSE)
  setwd(paste("Site","_",site,sep=""))
  for (h in 1:H) { #h = number of GCM X RCP X Times (usually 40 if using GCMs, 1 if using current/historical)
    scen<-temp[h] #pull out which scenario
    dir.create(paste0("Site","_",site,"_",scen), showWarnings = FALSE) #create a new directory with the site number and scenario name 
    setwd(paste("Site","_",site,"_",scen,sep="")) #reset the working directory into that new directory
    list1<-list() #make 6 blank lists
    list2<-list()
    list3<-list()
    list4<-list()
    list5<-list()
    list6<-list()
    for (k in 1:K) { #k=number of years
      data1L<-sw_weatherList[[i]][[h]][[k]] #pull out the list for the site, GCM, and year
      if (k < 6) { # putting the decades into different lists so we can resample them
        list1<-append(list1,data1L)}
      if (k > 25 & k < 31) { 
        list6<-append(list6,data1L)}
      if (k > 5 & k < 11) {
        list2<-append(list2,data1L)}
      if (k > 10 & k < 16) {
        list3<-append(list3,data1L)}
      if (k > 15 & k < 21) {
        list4<-append(list4,data1L)}
      if (k > 20 & k < 26) {
        list5<-append(list5,data1L)}}
   
    ##this piece of code extracts all of the PPT data in a list 
    # the sums the years within the list and creates a mean value for the list
    # then ranks the lists by PPT and pulls out the first year of the list
    # of the driest set of 5 years
    ll<-list(list1,list2,list3,list4,list5,list6)
    ranks<-data.frame(ID=1:6)
    for (f in 1:6) {# will loop through all the smaller lists in ll
      means<-vector() #create a blank vector called means
      yr<-ll[[f]][[1]]@year #extrace the first year of the list
      for (hf in 1:5) {# number of years in each of the small lists - right now 5
        g<-data.frame(ll[[f]][[hf]]@data) #extract the data for each year of the 5 yr list
        s1<-sum(g$PPT_cm) #sum the PPT data within the year
        s2<-ll[[f]][[1]]@year #extract the "year" of that year in list
        means<-append(means,s1) #append means() with summed PPT for year
      }
      gh<-mean(means) #calcualts the mean value of PPT for the 5 year interval
      ranks$YEAR1[f]<-yr #puts the first year of the 5 yr list in a df called ranks
      ranks$MEAN[f]<-gh #puts the avg ppt in df called ranks
    }
    
    f<-ranks[with(ranks,order(MEAN)),] #rank the df called ranks
    ###
    #the 5 year list with the lowest value is in the first row
    NEED<-f$YEAR1[[1]] #extract the first year of the driest 5yr list (used later as an identifier)
    ###  
    
    largelist<-rep(ll,12) # make a large list of all 6, 5yr lists repeated 12 times (total 360 years)
    
    smpl<-list()
    samplelist<-list()
    b<-1
    for (j in 1:length(largelist)) {
      if (j==1) {
        extract<-sample(largelist,1,replace=F)
        samplelist<-append(samplelist,extract)
      } else {
        extract<-sample(largelist,1,replace=F)
        if (extract[[1]][[1]]@year==NEED & samplelist[[j-b]][[1]]@year==NEED) {
          print(NEED)
          largelist<-append(largelist,extract)
          b<-b+1
        } else {
          samplelist<-append(samplelist,extract)}
      }}
    
    year<-1901 #reset year to start at 1901 for everything - makes runnig STEPWAT much easier (no changing input files...)
    for (t in 1:RE) { #t = number of resampled lists (ie 100 years -> t=10 300 years -> t=30)
      for (r in 1:INT) { # r = number of years in each list1,list2,list3 (ie 10years,5years,2years)
        x<-data.frame(samplelist[[t]][[r]]@data) # create a dataframe of the days within the year
        colnames(x)<-c("#DOY","Tmax_C","Tmin_C","PPT_cm") # relabel the columns names 
        rownames(x)<- NULL #make rownames null (need this or else will have an extra column)
        write.table(x, file=paste("weath.",year,sep=""), sep="\t", row.names=F,quote=F) #write your year file in your directory
        year<-year+1 #increment year
      }
    }
    setwd(paste(weather.dir,"Site","_",site,sep=""))}
  setwd(weather.dir) } #reset initial working directory}

  
  
###############################################################   
} else if (INT==5 & TYPE=="drought") {
############################################################### 

  

#################################################
########## 5 year resampling interval - 20 year drought not back-to-back ###########
#################################################
setwd(weather.dir)
up<-c(7,13,19,25,31,37,43,49,55,61,67,73,79,85,91,97,103,109,115,121)
low<-c(1,6,12,18,24,30,36,42,48,54,60,66,72,78,84,90,96,102,108,114)
#### Take the large list, resample, and make new data files ####
for (i in 1:S) { # i = number of sites
  site<-i
  dir.create(paste0("Site","_",site), showWarnings = FALSE)
  setwd(paste("Site","_",site,sep=""))
  for (h in 1:H) { #h = number of GCM X RCP X Times (usually 40 if using GCMs, 1 if using current/historical)
    scen<-temp[h] #pull out which scenario
    dir.create(paste0("Site","_",site,"_",scen), showWarnings = FALSE) #create a new directory with the site number and scenario name 
    setwd(paste("Site","_",site,"_",scen,sep="")) #reset the working directory into that new directory
    list1<-list() #make 6 blank lists
    list2<-list()
    list3<-list()
    list4<-list()
    list5<-list()
    list6<-list()
    for (k in 1:K) { #k=number of years
      data1L<-sw_weatherList[[i]][[h]][[k]] #pull out the list for the site, GCM, and year
      if (k < 6) { # putting the decades into different lists so we can resample them
        list1<-append(list1,data1L)}
      if (k > 25 & k < 31) { 
        list6<-append(list6,data1L)}
      if (k > 5 & k < 11) {
        list2<-append(list2,data1L)}
      if (k > 10 & k < 16) {
        list3<-append(list3,data1L)}
      if (k > 15 & k < 21) {
        list4<-append(list4,data1L)}
      if (k > 20 & k < 26) {
        list5<-append(list5,data1L)}}
 
    ###    
    ##this piece of code extracts all of the PPT data in a list 
    # the sums the years within the list and creates a mean value for the list
    # then ranks the lists by PPT and pulls out the first year of the list
    # of the driest set of 5 years
    ll<-list(list1,list2,list3,list4,list5,list6)
    ranks<-data.frame(ID=1:6)
    for (f in 1:6) {# will loop through all the smaller lists in ll
      means<-vector()
      yr<-ll[[f]][[1]]@year
      for (hf in 1:INT) {# number of years in each of the small lists - right now 5
        g<-data.frame(ll[[f]][[hf]]@data)
        s1<-sum(g$PPT_cm)
        s2<-ll[[f]][[1]]@year
        means<-append(means,s1)
      }
      gh<-mean(means)
      ranks$YEAR1[f]<-yr
      ranks$MEAN[f]<-gh
    }
    
    f<-ranks[with(ranks,order(MEAN)),]
    ###
    
    NEED<-f$YEAR1[[1]]
    ###  
    
    largelist<-rep(ll,20)
    
    smpl<-list()
    samplelist<-list()
    trashlist<-list()
    flag<-FALSE
    b<-1
    for (j in 1:length(largelist)) {
      # first sampling
      if (j==1) {
        extract<-sample(largelist,1,replace=F)
        samplelist<-append(samplelist,extract)
        if (extract[[1]][[1]]@year==NEED) {
          flag<-TRUE
        } 
        
      } else { #for all other sampling
        
        for (u in 1:length(up)) {
          #identified above as the upper and lower bounds of intervals where only one dry period would occur
          upper<-up[u]
          #print(upper)
          lower<-low[u]
          #print(lower)
          if (j>lower & j<upper) {
            extract<-sample(largelist,1,replace=F)
            
            if (flag==FALSE) {
              if (extract[[1]][[1]]@year==NEED) {
                flag<-TRUE
                if (samplelist[[j-b]][[1]]@year==NEED) {
                  print("Double")
                  trashlist<-append(trashlist,extract)
                  b<-b+1
                } else {
                  samplelist<-append(samplelist,extract)
                }} else {
                  samplelist<-append(samplelist,extract)}}
            
            if (flag==TRUE) {
              if (extract[[1]][[1]]@year==NEED) {
                print("TRASH")
                trashlist<-append(trashlist,extract)
                b<-b+1
              } else {
                samplelist<-append(samplelist,extract)
              }} 
            
          }
          flag<-FALSE}}}
    
    year<-1901 #reset year to start at 1901 for everything - makes runnig STEPWAT much easier (no changing input files...)
    for (t in 1:RE) { #t = number of resampled lists (ie 100 years -> t=10 300 years -> t=30)
      for (r in 1:INT) { # r = number of years in each list1,list2,list3 (ie 10years,5years,2years)
        x<-data.frame(samplelist[[t]][[r]]@data) # create a dataframe of the days within the year
        colnames(x)<-c("#DOY","Tmax_C","Tmin_C","PPT_cm") # relabel the columns names 
        rownames(x)<- NULL #make rownames null (need this or else will have an extra column)
        write.table(x, file=paste("weath.",year,sep=""), sep="\t", row.names=F,quote=F) #write your year file in your directory
        year<-year+1 #increment year
      }
    }
    setwd(paste(weather.dir,"Site","_",site,sep=""))}
  setwd(weather.dir) } #reset initial working directory}



############################################################### 
} else if (INT==1 & TYPE=="basic") {
############################################################### 

  

#################################################
########## 1 year resampling interval - basic ###########
#################################################
setwd(weather.dir)
#### Take the large list, resample, and make new data files ####
for (i in 1:S) { # i = number of sites
  site<-i
  dir.create(paste0("Site","_",site), showWarnings = FALSE)
  setwd(paste("Site","_",site,sep=""))
  for (h in 1:H) { #h = number of GCM X RCP X Times (usually 40 if using GCMs, 1 if using current/historical)
    scen<-temp[h] #pull out which scenario
    dir.create(paste0("Site","_",site,"_",scen), showWarnings = FALSE) #create a new directory with the site number and scenario name 
    setwd(paste("Site","_",site,"_",scen,sep="")) #reset the working directory into that new directory
    list<-list() #make 6 blank lists
    for (k in 1:K) { #k=number of years
      data1L<-sw_weatherList[[i]][[h]][[k]] #pull out the list for the site, GCM, and year
      list<-append(list,data1L)
    }
    samplelist<-sample(list,FIN,replace=T) #resample that large list (10 for 100yrs, 30 for 300yrs)
    year<-1901 #reset year to start at 1901 for everything - makes runnig STEPWAT much easier (no changing input files...)
    for (t in 1:300) { #t = number of resampled lists (ie 100 years -> t=10 300 years -> t=30)
      x<-data.frame(samplelist[[t]]@data) # create a dataframe of the days within the year
      colnames(x)<-c("#DOY","Tmax_C","Tmin_C","PPT_cm") # relabel the columns names 
      rownames(x)<- NULL #make rownames null (need this or else will have an extra column)
      write.table(x, file=paste("weath.",year,sep=""), sep="\t", row.names=F,quote=F) #write your year file in your directory
      year<-year+1 #increment year
    }
    setwd(paste(weather.dir,"Site","_",site,sep=""))}
  setwd(weather.dir)} # reset initial working directory
  
  

############################################################### 
} else if (INT==30 & TYPE=="basic") {
############################################################### 
  
  
  
#################################################
########## 30 year resampling interval - basic ###########
#################################################
setwd(weather.dir)
#### Take the large list, resample, and make new data files ####
for (i in 1:S) { # i = number of sites
  site<-i
  dir.create(paste0("Site","_",site), showWarnings = FALSE)
  setwd(paste("Site","_",site,sep=""))
  for (h in 1:H) { #h = number of GCM X RCP X Times (usually 40 if using GCMs, 1 if using current/historical)
    scen<-temp[h] #pull out which scenario
    dir.create(paste0("Site","_",site,"_",scen), showWarnings = FALSE) #create a new directory with the site number and scenario name 
    setwd(paste("Site","_",site,"_",scen,sep="")) #reset the working directory into that new directory
    list<-list() #make 6 blank lists
    data1L<-sw_weatherList[[i]][[h]] #pull out the list for the site, GCM, and year
    samplelist<-c(data1L,data1L,data1L,data1L,data1L,data1L,data1L,data1L,data1L,data1L)
    year<-1901 #reset year to start at 1901 for everything - makes runnig STEPWAT much easier (no changing input files...)
    for (t in 1:FIN) { #t = number of resampled lists (ie 100 years -> t=10 300 years -> t=30)
      x<-data.frame(samplelist[[t]]@data) # create a dataframe of the days within the year
      colnames(x)<-c("#DOY","Tmax_C","Tmin_C","PPT_cm") # relabel the columns names 
      rownames(x)<- NULL #make rownames null (need this or else will have an extra column)
      write.table(x, file=paste("weath.",year,sep=""), sep="\t", row.names=F,quote=F) #write your year file in your directory
      year<-year+1 #increment year
    }
    setwd(paste(weather.dir,"Site","_",site,sep=""))}
  setwd(weather.dir)} # reset initial working directory}

    
###############################################################   
} else if (INT==10 & TYPE=="basic") {
###############################################################   
  
  
  
#################################################
########## 10 year resampling interval ###########
#################################################
  setwd(weather.dir)
  #### Take the large list, resample, and make new data files ####
  for (i in 1:S) { # i = number of sites
    site<-i
    dir.create(paste0("Site","_",site), showWarnings = FALSE)
    setwd(paste("Site","_",site,sep=""))
    for (h in 1:H) { #h = number of GCM X RCP X Times (usually 40 if using GCMs, 1 if using current/historical)
      scen<-temp[h] #pull out which scenario
      dir.create(paste0("Site","_",site,"_",scen), showWarnings = FALSE) #create a new directory with the site number and scenario name 
      setwd(paste("Site","_",site,"_",scen,sep="")) #reset the working directory into that new directory
      list1<-list() #make 3 blank lists
      list2<-list()
      list3<-list()
      for (k in 1:K) { #k=number of years
        data1L<-sw_weatherList[[i]][[h]][[k]] #pull out the list for the site, GCM, and year
        if (k < 11) { # putting the decades into different lists so we can resample them
          list1<-append(list1,data1L)}
        if (k > 20) { 
          list3<-append(list3,data1L)}
        if (k > 10 & k < 21) {list2<-append(list2,data1L)}
      }
      largelist<-list(list1,list2,list3) # create a large list of the decadal lists
      samplelist<-sample(largelist,RE,replace=T) #resample that large list (10 for 100yrs, 30 for 300yrs)
      year<-1901 #reset year to start at 1901 for everything - makes runnig STEPWAT much easier (no changing input files...)
      for (t in 1:RE) { #t = number of resampled lists (ie 100 years -> t=10 300 years -> t=30)
        for (r in 1:INT) { # r = number of years in each list1,list2,list3 (ie 10years)
          x<-data.frame(samplelist[[t]][[r]]@data) # create a dataframe of the days within the year
          colnames(x)<-c("#DOY","Tmax_C","Tmin_C","PPT_cm") # relabel the columns names 
          rownames(x)<- NULL #make rownames null (need this or else will have an extra column)
          write.table(x, file=paste("weath.",year,sep=""), sep="\t", row.names=F,quote=F) #write your year file in your directory
          year<-year+1 #increment year
        }
      }
      setwd(paste(weather.dir,"Site","_",site,sep=""))}
    setwd(weather.dir)} # reset initial working directory}
 
  
  


############################################################### 
} else {
  print(paste(INT,"&",TYPE,"pairing does not exist"))}
############################################################### 
  
if (email.flag==TRUE) {
  sendmail(email,"Weather Assembly Done")}
