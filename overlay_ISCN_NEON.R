rm(list = ls())
library(tidyverse)
library(sf)
library(terra)

{
  #convert soc_30cm_df lat long to UTM
soc_30cm_df <-  read_csv("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/iscn/ISCN_30cm_USA.csv") %>% 
  select(-c("dataset_name_sub" ,"n","rep",
            "AFFGEOID","GEOID" ,"NAME",
            "profile_top_cm" ,"profile_bottom_cm"))
#33881
# install.packages("PBSmapping")
library("PBSmapping")
soc_30cm_df$north=1
soc_30cm_df$east=1
soc_30cm_df$utm=1

for (i in 1:nrow(soc_30cm_df)) {
  latlong=soc_30cm_df[i,] %>%
    select(X=long,Y=lat)
  attr(latlong, "projection") <- "LL"
  latlongUTM <- convUL(latlong, km=FALSE)
  
  utmzone=  attr(latlongUTM, "zone")

  soc_30cm_df[i,7]=latlongUTM$Y
  soc_30cm_df[i,8]=latlongUTM$X
  soc_30cm_df[i,9]=utmzone
 }

 write_csv(soc_30cm_df, "/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/iscn/ISCN_30cm_USA_UTM.csv")
}



