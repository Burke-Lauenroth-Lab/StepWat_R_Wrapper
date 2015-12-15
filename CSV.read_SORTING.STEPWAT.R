###################################################
### Stepwat: CSV reading and sorting            ###
### Author: Trace E Martyn                      ###
### Date: 12.14.2015                            ###
###################################################

###################################################
## Read in all the files and make mega database  ##
## Sort the output for each year and each Rgroup ##
##    based the GCM or Site specific output and  ##
##    identify the max, min and median values.   ##
###################################################

####################################################
## Must open, save, and close all csv files     ####
##    in Excel before running code              ####
####################################################

######################################################################
#### Must open and save all CSVs in Excel until problem is solved ####
######################################################################
#### assemble all of the bmassavg....csv's into a file structure  ####
####    that as a single folder for each site labeled 'Site.1'    ####
####    for each site in run. Each folder contains all the bmass  ####
####    csv's labeled as the wrapper labeled them. This will need ####
####    to be adjusted for output that includes disturbance and   ####
####    grazing because of the difference in the naming structure ####
######################################################################

######################################################################
SITE<-1:10
RCP<-c("RCP45","RCP85")
YEARS<-c("50years","90years")
GCM<-c("ACCESS1-0","CanESM2","CESM1-CAM5","CMCC-CM","CNRM-CM5","CSIRO-Mk3-6-0","HadGEM2-ES","IPSL-CM5A-MR","MIROC5","NorESM1-M","Current")

# create empty dataframe
STEPWAT.300<-data.frame()

for (s in SITE) { # for each site
  setwd(paste0("C:/Users/litak/Dropbox/STEPWAT.Output/Site.",s,"/"))
  for (g in GCM) { # for each GCM
    if (g=="Current") { # special case for Current
      name<-paste("bmassavg.Site",s,g,sep=".")
      df<-read.csv(paste0(name,".csv"))
      df$Site<-s
      df$Period<-0
      df$RCP<-0
      df$GCM<-g
      assign(name,df)
      STEPWAT.300<-rbind(STEPWAT.300,df)} else {
        for (r in RCP) { # for each RCP
          for (y in YEARS) { # for each period 2060 (50) or 2100 (90)
            name<-paste("bmassavg.Site",s,g,y,r,sep=".")
            df<-read.csv(paste0(name,".csv"))
            df$Site<-s
            df$Period<-y
            df$RCP<-r
            df$GCM<-g
            assign(name,df)
            STEPWAT.300<-rbind(STEPWAT.300,df)}}}} 
  # save R.Data image for each site - just in case of failing code
  save.image(paste("Site",s,"RData",sep="."))}

# set where you want large csv to be exported
setwd("C:/Users/litak/Dropbox/Dropbox")

#write csv
write.csv(STEPWAT.300,"StepWat.300yrs.AllSites.csv")





########### GCM Variability ####################################
########## make a new dataframe of median max and min values ###
Stepwat.DF<-read.csv("StepWat.300yrs.AllSites.csv")
GCM<-unique(Stepwat.DF$GCM) 
GCM<-unique(GCM[1:10])
Rgroup<-c("sagebrush","a.cool.forb","a.warm.forb","p.cool.forb","p.warm.forb","a.cool.grass","a.warm.grass","p.cool.grass","p.warm.grass","shrub")
Site<-unique(Stepwat.DF$Site)
Period<-unique(Stepwat.DF$Period)
Period<-Period[1:2]
RCP<-unique(Stepwat.DF$RCP)
RCP<-RCP[1:2]
Year<-unique(Stepwat.DF$Year)
Stepwat.Sorted<-data.frame(ID=1:12001)
c<-1

for (p in Period) { # for each period 2060 or 2100
  print(p)
  for (s in RCP) { # for each RCP
    print(s)
    for (y in Year) { # for each year
      for (r in Rgroup) { # for each group
        DF.new<-data.frame(ID=1:10)
        b<-1
        for( g in GCM) { #for each GCM
          yearly.r<-Stepwat.DF[which(Stepwat.DF$GCM==g & Stepwat.DF$Year ==y & Stepwat.DF$RCP==s & Stepwat.DF$Period==p),r]
          m.yearly.r<-mean(yearly.r,na.rm=T)
          DF.new$Year[b]<-y
          DF.new$RGroup[b]<-r
          DF.new$GCM[b]<-g
          DF.new$Bmass[b]<-m.yearly.r
          DF.new
          b<-b+1}
        DF.temp<-DF.new[order(DF.new$Bmass),]
        Stepwat.Sorted$RCP[c]<-s
        Stepwat.Sorted$Period[c]<-p
        Stepwat.Sorted$Year[c]<-y
        Stepwat.Sorted$RGroup[c]<-r
        Stepwat.Sorted$GCM.min[c]<-DF.temp$GCM[1]
        Stepwat.Sorted$Bmass.min[c]<-DF.temp$Bmass[1]
        Stepwat.Sorted$GCM.max[c]<-DF.temp$GCM[10]
        Stepwat.Sorted$Bmass.max[c]<-DF.temp$Bmass[10]
        Stepwat.Sorted$GCM.med1[c]<-DF.temp$GCM[5]
        Stepwat.Sorted$Bmass.med1[c]<-DF.temp$Bmass[5]
        Stepwat.Sorted$GCM.med2[c]<-DF.temp$GCM[6]
        Stepwat.Sorted$Bmass.med2[c]<-DF.temp$Bmass[6]
        c<-c+1}
    }
  }
}

#reorder to make it easier to make figures 
Stepwat.Sorted<-Stepwat.Sorted[order(Stepwat.Sorted$RCP,Stepwat.Sorted$Period,Stepwat.Sorted$RGroup,Stepwat.Sorted$Year),]

#write csv
write.csv(Stepwat.Sorted,"Stepwat.DF.Sorted.csv")




############# Current GCM ####################################
Stepwat.DF<-read.csv("StepWat.300yrs.AllSites.csv")
GCM<-"Current"
Rgroup<-c("sagebrush","a.cool.forb","a.warm.forb","p.cool.forb","p.warm.forb","a.cool.grass","a.warm.grass","p.cool.grass","p.warm.grass","shrub")
Site<-unique(Stepwat.DF$Site)
Period<-unique(Stepwat.DF$Period)
Period<-0
RCP<-unique(Stepwat.DF$RCP)
RCP<-0
Year<-unique(Stepwat.DF$Year)
Stepwat.Sorted.Current<-data.frame(ID=1:3000)
c<-1

for (p in Period) { # for each period 2060 or 2100
  print(p)
  for (s in RCP) { # for each RCP
    print(s)
    for (y in Year) { # for each year
      for (r in Rgroup) { # for each group
        DF.new<-data.frame(ID=1:10)
        b<-1
        for( g in GCM) { #for each GCM
          yearly.r<-Stepwat.DF[which(Stepwat.DF$GCM==g & Stepwat.DF$Year ==y & Stepwat.DF$RCP==s & Stepwat.DF$Period==p),r]
          m.yearly.r<-mean(yearly.r,na.rm=T)
          DF.new$Year[b]<-y
          DF.new$RGroup[b]<-r
          DF.new$GCM[b]<-g
          DF.new$Bmass[b]<-m.yearly.r
          DF.new
          b<-b+1}
        DF.temp<-DF.new[order(DF.new$Bmass),]
        Stepwat.Sorted.Current$RCP[c]<-s
        Stepwat.Sorted.Current$Period[c]<-p
        Stepwat.Sorted.Current$Year[c]<-y
        Stepwat.Sorted.Current$RGroup[c]<-r
        Stepwat.Sorted.Current$GCM.min[c]<-DF.temp$GCM[1]
        Stepwat.Sorted.Current$Bmass.min[c]<-DF.temp$Bmass[1]
        Stepwat.Sorted.Current$GCM.max[c]<-DF.temp$GCM[10]
        Stepwat.Sorted.Current$Bmass.max[c]<-DF.temp$Bmass[10]
        Stepwat.Sorted.Current$GCM.med1[c]<-DF.temp$GCM[5]
        Stepwat.Sorted.Current$Bmass.med1[c]<-DF.temp$Bmass[5]
        Stepwat.Sorted.Current$GCM.med2[c]<-DF.temp$GCM[6]
        Stepwat.Sorted.Current$Bmass.med2[c]<-DF.temp$Bmass[6]
        c<-c+1}
    }
  }
}

#reorder to make it easier to make figures 
Stepwat.Sorted.Current<-Stepwat.Sorted.Current[order(Stepwat.Sorted.Current$RGroup,Stepwat.Sorted.Current$Year),]

#write csv
write.csv(Stepwat.Sorted.Current,"Stepwat.DF.Sorted.Current.csv")




################ Site Variability ###############################
Stepwat.DF<-read.csv("StepWat.300yrs.AllSites.csv")
########## make a new dataframe of median max and min values ###
GCM<-unique(Stepwat.DF$GCM) 
GCM<-unique(GCM[1:10])
Rgroup<-c("sagebrush","a.cool.forb","a.warm.forb","p.cool.forb","p.warm.forb","a.cool.grass","a.warm.grass","p.cool.grass","p.warm.grass","shrub")
Site<-1:10
Period<-unique(Stepwat.DF$Period)
Period<-Period[1:2]
RCP<-unique(Stepwat.DF$RCP)
RCP<-RCP[1:2]
Year<-1:300
Stepwat.Sorted.Site<-data.frame(ID=1:12000)
c<-1
for (p in Period) { # for each period 2060 (50) or 2100 (90)
  print(p)
  for (s in RCP) { # for each RCP
    print(s)
    for (y in Year) { # for each year
      for (r in Rgroup) { # for each group
        DF.new<-data.frame(ID=1:10)
        b<-1
        for (i in Site) { # for each site
          yearly.r<-Stepwat.DF[which(Stepwat.DF$Site==i & Stepwat.DF$Year ==y & Stepwat.DF$RCP==s & Stepwat.DF$Period==p),r]
          m.yearly.r<-mean(yearly.r)
          DF.new$Year[b]<-y
          DF.new$RGroup[b]<-r
          DF.new$Site[b]<-i
          DF.new$Bmass[b]<-m.yearly.r
          DF.new
          b<-b+1}
        DF.temp<-DF.new[order(DF.new$Bmass),]
        Stepwat.Sorted.Site$RCP[c]<-s
        Stepwat.Sorted.Site$Period[c]<-p
        Stepwat.Sorted.Site$Year[c]<-y
        Stepwat.Sorted.Site$RGroup[c]<-r
        Stepwat.Sorted.Site$Site.min[c]<-DF.temp$Site[1]
        Stepwat.Sorted.Site$Bmass.min[c]<-DF.temp$Bmass[1]
        Stepwat.Sorted.Site$Site.max[c]<-DF.temp$Site[10]
        Stepwat.Sorted.Site$Bmass.max[c]<-DF.temp$Bmass[10]
        Stepwat.Sorted.Site$Site.med1[c]<-DF.temp$Site[5]
        Stepwat.Sorted.Site$Bmass.med1[c]<-DF.temp$Bmass[5]
        Stepwat.Sorted.Site$Site.med2[c]<-DF.temp$Site[6]
        Stepwat.Sorted.Site$Bmass.med2[c]<-DF.temp$Bmass[6]
        c<-c+1}
    }
  }
}

#reorder to make it easier to make figures 
Stepwat.Sorted.Site<-Stepwat.Sorted.Site[order(Stepwat.Sorted.Site$RCP,Stepwat.Sorted.Site$Period,Stepwat.Sorted.Site$RGroup,Stepwat.Sorted.Site$Year),]

#write csv
write.csv(Stepwat.Sorted.Site,"Stepwat.DF.Sorted.Site.csv")





######## Current Site Variability #######################
########## make a new dataframe of median max and min values ###
GCM<-"Current"
Rgroup<-c("sagebrush","a.cool.forb","a.warm.forb","p.cool.forb","p.warm.forb","a.cool.grass","a.warm.grass","p.cool.grass","p.warm.grass","shrub")
Site<-c(1,3)
Period<-0
RCP<-0
Year<-1:300
Stepwat.Sorted.Site.Current<-data.frame(ID=1:3001)
c<-1
for (p in Period) { # for each period 2060 or 2100
  print(p)
  for (s in RCP) { # for each RCP
    print(s)
    for (y in Year) { # for each year
      for (r in Rgroup) {
        DF.new<-data.frame(ID=1:10)
        b<-1# for each group
        for (i in Site) { # for each site
          yearly.r<-Stepwat.DF[which(Stepwat.DF$Site==i & Stepwat.DF$Year ==y & Stepwat.DF$RCP==s & Stepwat.DF$Period==p),r]
          m.yearly.r<-mean(yearly.r)
          DF.new$Year[b]<-y
          DF.new$RGroup[b]<-r
          DF.new$Site[b]<-i
          DF.new$Bmass[b]<-m.yearly.r
          b<-b+1}
        DF.temp<-DF.new[order(DF.new$Bmass),]
        Stepwat.Sorted.Site.Current$RCP[c]<-s
        Stepwat.Sorted.Site.Current$Period[c]<-p
        Stepwat.Sorted.Site.Current$Year[c]<-y
        Stepwat.Sorted.Site.Current$RGroup[c]<-r
        Stepwat.Sorted.Site.Current$Site.min[c]<-DF.temp$Site[1]
        Stepwat.Sorted.Site.Current$Bmass.min[c]<-DF.temp$Bmass[1]
        Stepwat.Sorted.Site.Current$Site.max[c]<-DF.temp$Site[10]
        Stepwat.Sorted.Site.Current$Bmass.max[c]<-DF.temp$Bmass[10]
        Stepwat.Sorted.Site.Current$Site.med1[c]<-DF.temp$Site[5]
        Stepwat.Sorted.Site.Current$Bmass.med1[c]<-DF.temp$Bmass[5]
        Stepwat.Sorted.Site.Current$Site.med2[c]<-DF.temp$Site[6]
        Stepwat.Sorted.Site.Current$Bmass.med2[c]<-DF.temp$Bmass[6]
        c<-c+1}
    }
  }
}

#reorder to make it easier to make figures 
Stepwat.Sorted.Site.Current<-Stepwat.Sorted.Site.Current[order(Stepwat.Sorted.Site.Current$RGroup,Stepwat.Sorted.Site.Current$Year),]

#write csv
write.csv(Stepwat.Sorted.Site.Current,"Stepwat.DF.Sorted.Site.Current.csv")


