# PlateMaker


## Description:
An R-package that generates 96-well plate templates. 

## Install directions:
devtools::install_github(repo="zakrobinson/PlateMaker")


## Example function use for PlateMaker
 
library(PlateMaker)

samp_IDs<-paste("ind", 1:400,sep="-")

plate_map_extract(ids = samp_IDs,wtfile = F,des = "Project_1",path = getwd(), partial = T)