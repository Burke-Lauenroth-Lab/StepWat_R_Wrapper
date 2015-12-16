## Clone the latest version of StepWat from github (code below) and copy all the files into
##		folders for unique, individual sites.

###################################
Install Stepwat from github:
# change working directory to Desktop
> cd Desktop
# clone Stepwat from github.com to a folder called Stepwat on the Desktop
> git clone https://github.com/Burke-Lauenroth-Lab/StepWat.git —-branch SoilWat31_drs —-single-branch Stepwat 
# change directory into new folder
> cd Stepwat
# update the submodule
> git submodule update —-init —-recursive
# build Stepwat
> make
###################################