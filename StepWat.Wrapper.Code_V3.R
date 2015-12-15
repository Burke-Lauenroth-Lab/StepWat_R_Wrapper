###################################################
### Stepwat Wrapper file                        ###
### Author: Trace E Martyn                      ###
### Date: 12.14.2015                            ###
###################################################

####################################################
## Use this code to loop through all of the     ####
##    sites and GCM/PERIOD/RCP combinations     ####
####################################################

setwd(directory)

for (s in sites) { # loop through all the sites
  for (g in GCM) { # loop through all the GCMs
    
    
    # Go to the weather directory
    setwd(paste(weather.dir,"Site_",s,sep=""))
    
    # if on the "Current" GCM read the weather data into randomdata
    if (g=="Current") {
      setwd(paste("Site_",s,"_",g,sep=""))
      weath.read<-paste(weather.dir,"Site_",s,"/Site_",s,"_",g,sep="")
      
      # identify the directory the weather will be pasted into        
      weather.dir2<-paste(directory,"Stepwat.Site.",s,"/testing.sagebrush.MT_drs/Stepwat_Inputs/Input/sxw/Input/randomdata/",sep="")
      
      # copy the weather data into the randomdata folder
      if (TYPE=="basic" || TYPE=="drought" || TYPE=="back") {
        # copy the weather data into the randomdata folder
        system(paste("cp -a ",weath.read,"/. ",weather.dir2,sep=""))
        } else
      if (TYPE=="markov") {
        system(paste("cp ",weath.read,"/mkv_covar.in ",weather.dir2,sep=""))
        system(paste("cp ",weath.read,"/mkv_prob.in ",weather.dir2,sep=""))
      }
      
      if (dist.graz.flag == T) {
        for (dst in dist.freq) {
          for (grz in graz.freq) {
            setwd(paste0(dist.directory))
            dist.graz.name<-paste0("rgroup.freq",dst,".graz",grz,".in")
            system(paste0("cp ",dist.graz.name," ",directory,"Stepwat.Site.",s,"/testing.sagebrush.MT_drs/Stepwat_Inputs/Input/"))
            setwd(paste0(directory,"Stepwat.Site.",s,"/testing.sagebrush.MT_drs/Stepwat_Inputs/Input/"))
            system("rm rgroup.in")
            system(paste0("mv ",dist.graz.name," rgroup.in"))
            
            
            # change directory to the executable directory
            setwd(paste(directory,"Stepwat.Site.",s,"/testing.sagebrush.MT_drs/Stepwat_Inputs",sep=""))
            # run stepwat
            system("./stepwat -f files.in -ssxwdebug.in")
            
            # change directory to "Output" folder
            setwd("Output")
            
              # identify the name of the biomass output file
              name.bmass.csv<-paste("bmassavg.Site",s,g,"D",dst,"G",grz,"csv",sep=".")
              name.mort.csv<-paste("mortavg.Site",s,g,"D",dst,"G",grz,"csv",sep=".")
              name.stdebug.sqlite<-paste("stdebug.Site",s,g,"D",dst,"G",grz,"sqlite3",sep=".")
              
              # rename the bmassavg.csv
              system(paste("mv bmassavg.csv ",name.bmass.csv,sep=""))
              system(paste("mv mortavg.csv ",name.mort.csv,sep=""))
              system(paste("mv stdebug.sqlite3 ",name.stdebug.sqlite,sep=""))
            }
            
           
            print(paste0("DIST.GRAZ D",dst,".G",grz," DONE"))
            }
            
          }
          
          else if (dist.graz.flag ==F) {
            
            # change directory to the executable directory
            setwd(paste(directory,"Stepwat.Site.",s,"/testing.sagebrush.MT_drs/Stepwat_Inputs",sep=""))
            # run stepwat
            system("./stepwat -f files.in -ssxwdebug.in")
            
            # change directory to "Output" folder
            setwd("Output")
            
            
            # identify the name of the biomass output file
            name.bmass.csv<-paste("bmassavg.Site",s,g,"csv",sep=".")
            name.mort.csv<-paste("mortavg.Site",s,g,"csv",sep=".")
            name.stdebug.sqlite<-paste("stdebug.Site",s,g,"sqlite3",sep=".")
            
            # rename the bmassavg.csv
            system(paste("mv bmassavg.csv ",name.bmass.csv,sep=""))
            system(paste("mv mortavg.csv ",name.mort.csv,sep=""))
            system(paste("mv stdebug.sqlite3 ",name.stdebug.sqlite,sep=""))
            }
      # for all other GMC/year/RCP read the weather data into randomdata    
    } else if (g!="Current"){
      
      for (y in YEARS) { # loop through all the time periods 50 or 90
        for (r in RCP) { # loop through all the RCP
          
          # Go to the weather directory
          setwd(paste(weather.dir,"Site_",s,sep=""))
          
          setwd(paste("Site_",s,"_hybrid-delta.",y,".",r,".",g, sep=""))
          weath.read<-paste(weather.dir,"Site_",s,"/Site_",s,"_hybrid-delta.",y,".",r,".",g, sep="")
          
          
          
          # identify the directory the weather will be pasted into        
          weather.dir2<-paste(directory,"Stepwat.Site.",s,"/testing.sagebrush.MT_drs/Stepwat_Inputs/Input/sxw/Input/randomdata/",sep="")
          
          # copy the weather data into the randomdata folder
          if (TYPE=="basic" || TYPE=="drought" || TYPE=="back") {
            # copy the weather data into the randomdata folder
            system(paste("cp -a ",weath.read,"/. ",weather.dir2,sep=""))
            } else
              if (TYPE=="markov") {
                system(paste("cp ",weath.read,"/mkv_covar.in ",weather.dir2,sep=""))
                system(paste("cp ",weath.read,"/mkv_prob.in ",weather.dir2,sep=""))
              }
          
          if (dist.graz.flag == T) {
            for (dst in dist.freq) {
              for (grz in graz.freq) {
                setwd(paste0(dist.directory))
                dist.graz.name<-paste0("rgroup.freq",dst,".graz",grz,".in")
                system(paste0("cp ",dist.graz.name," ",directory,"Stepwat.Site.",s,"/testing.sagebrush.MT_drs/Stepwat_Inputs/Input/"))
                setwd(paste0(directory,"Stepwat.Site.",s,"/testing.sagebrush.MT_drs/Stepwat_Inputs/Input/"))
                system("rm rgroup.in")
                system(paste0("mv ",dist.graz.name," rgroup.in"))
                
                
                # change directory to the executable directory
                setwd(paste(directory,"Stepwat.Site.",s,"/testing.sagebrush.MT_drs/Stepwat_Inputs",sep=""))
                # run stepwat
                system("./stepwat -f files.in -ssxwdebug.in")
                
                # change directory to "Output" folder
                setwd("Output")
                
                # identify the name of the biomass output file
                name.bmass.csv<-paste("bmassavg.Site",s,g,y,r,"D",dst,"G",grz,"csv",sep=".")
                name.mort.csv<-paste("mortavg.Site",s,g,y,r,"D",dst,"G",grz,"csv",sep=".")
                name.stdebug.sqlite<-paste("stdebug.Site",s,g,y,r,"D",dst,"G",grz,"sqlite3",sep=".")
                
                # rename the bmassavg.csv
                system(paste("mv bmassavg.csv ",name.bmass.csv,sep=""))
                system(paste("mv mortavg.csv ",name.mort.csv,sep=""))
                system(paste("mv stdebug.sqlite3 ",name.stdebug.sqlite,sep=""))
              }
              
              
              print(paste0("DIST.GRAZ D",dst,".G",grz," DONE"))
              }
            
          }
      
      else if (dist.graz.flag ==F) {
        
        # change directory to the executable directory
        setwd(paste(directory,"Stepwat.Site.",s,"/testing.sagebrush.MT_drs/Stepwat_Inputs",sep=""))
        # run stepwat
        system("./stepwat -f files.in -ssxwdebug.in")
        
        # change directory to "Output" folder
        setwd("Output")
        
        
        # identify the name of the biomass output file
        name.bmass.csv<-paste("bmassavg.Site",s,g,y,r,"csv",sep=".")
        name.mort.csv<-paste("mortavg.Site",s,g,y,r,"csv",sep=".")
        name.stdebug.sqlite<-paste("stdebug.Site",s,g,y,r,"sqlite3",sep=".")
        
        # rename the bmassavg.csv
        system(paste("mv bmassavg.csv ",name.bmass.csv,sep=""))
        system(paste("mv mortavg.csv ",name.mort.csv,sep=""))
        system(paste("mv stdebug.sqlite3 ",name.stdebug.sqlite,sep=""))
        }
      
            print(paste("RCP ",r," DONE",sep=""))
            }
        #print statement for when model done with that GCM
        print(paste("YEAR ",y," DONE",sep=""))
      }
      
    }
    print(paste("GCM ",g," DONE",sep=""))
  }
  # print statement for when model done with Site
  print(paste("Site ",s," Done",sep=""))
  
  # if the email flag is set to true - send email to the email listed in 
  #     main code file for each site
  if (email.flag==TRUE) {
    sendmail(email,"StepWat",paste("Site ",s," Done"))} else {next}
}


# if the email flag is set to true - send email when all sites are donw
if (email.flag==TRUE) {
  sendmail(email,"StepWat Runs Done")}


