### Reset workspace

rm(list = ls())

#setwd to home, then check which pc.
setwd("~")
whichpc=getwd()
cat(whichpc)
if(whichpc=="/Users/zwang61"){
  user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
}else{
  user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
}

#FINAL products sls,spc,mgp 30cm soil SOC and location in UTM-----
finalproduct <-paste0(user_wd,"/NEON-AOP/data/hs_lai/NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC.csv")
file.exists(finalproduct)

#extract 10047 spc and 00096 mgp: 
#Estimated organic carbon content (estimatedOC) /unit: gramsPerKilogram

#10086 sls_soilChemistry:organicCPercent: 
#Percent organic carbon in a sample on a dry weight basis/unit: percent %

#Estimated organic carbon content (estimatedOC) is calculated by 
#subtracting the carbon content of calcium carbonate (% gravimetric) measured in the sample from the total carbon content (% gravimetric). 
#see NEON USER GUIDE TO SOIL PHYSICAL AND CHEMICAL PROPERTIES, MEGAPIT (DP1.00096.001)

library(tidyverse)
library(geoNEON)
NEON_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ6aHVvbmFuLndhbmdAZW1vcnkuZWR1Iiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxNzg3Njk4ODU4LCJpYXQiOjE2MzAwMTg4NTgsImVtYWlsIjoiemh1b25hbi53YW5nQGVtb3J5LmVkdSJ9.Ug3DuSLWLT0NsZy5pHKs8Wh51C9-tjxoCccbhrUW0m49n1GikPp0EQ3jZc3c6FFzRpDk04UXA-k4fECyxzqnmQ"

# wd <- "/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/hs_lai/" #This will depend on your local environment
# setwd(wd)

#step 1 Calculate precise geographic location
#10047 spc estimatedOC
#00096 mgp estimatedOC
#10086 sls organicCPercent
dir <- file.path(paste0(user_wd,"/NEON-AOP/data/neon_data/") )
portalFolder <- "/portal"
outputs <- "/compiled"

#start spc ------
#spc pits location----initial characterization 
spc_spatial <- read.csv(paste0(dir, outputs, "/spatial/spatial_spc.csv"), 
                        header = T, na.strings = "", stringsAsFactors = F)
source(paste0(user_wd,"/NEON-AOP/code/My_functions_R/spc_Cal_PitsLocation.R"))
spc_pits_spatial <- spc_Cal_PitsLocation()

# spc CONVERTING HORIZONS TO FIXED DEPTH 0-30 cm: weighted mean, na.rm=TRUE--------------------------------
# using_aqp=TRUE
# if(using_aqp ==TRUE){
# This approach uses the aqp package to convert the by-horizon data to fixed interval
# For this to work, need to turn the dataset into a 'soil profile collection'
# download 'aqp' pacakge, but don't attach it, lots of conflicts with dplyr
# devtools::install_github("ncss-tech/aqp", dependencies=FALSE, upgrade=FALSE, build=FALSE)
dir <- file.path(paste0(user_wd,"/NEON-AOP/data/neon_data/") )
spc_perbiogeo <- read.csv(paste0(dir, portalFolder, "/filesToStack10047/stackedFiles/spc_biogeochem.csv"), 
                          header = T, na.strings = "", stringsAsFactors = F)
dim(spc_perbiogeo)#update to 3037 from #2990  126
sum(is.na(spc_perbiogeo$estimatedOC)) # 0 na: all of them have estimated OCsum(is.na(spc$horizonName_new)) # none are empty, good
length(unique(spc_perbiogeo$nrcsDescriptionID)) #update to 727 # 715 pits, from 715 plots

#copy it as spc to convert to soilprofile
spc=spc_perbiogeo
# set up soil collections
aqp::depths(spc) <- nrcsDescriptionID ~ biogeoTopDepth + biogeoBottomDepth
aqp::hzdesgnname(spc) <- 'horizonName'
aqp::site(spc) <- ~ domainID + siteID + plotID+collectDate

# check new SPC
spc
#http://ncss-tech.github.io/AQP/aqp/dealing-with-bad-data.html
bad.chk <- aqp::checkHzDepthLogic(spc)
# how many profiles (pits)/plots per siteID? ranges from 6-26
table(spc$siteID)

# visual check for a couple of sites
par(mar = c(0, 0, 3, 0))
aqp::plotSPC(subset(spc, spc$siteID == 'SJER'), color = 'phH2o', name.style = 'center-center', hz.depths = TRUE, plot.depth.axis = FALSE)
aqp::plotSPC(subset(spc, spc$siteID == 'BART'), color = 'phH2o', name.style = 'center-center', hz.depths = TRUE, plot.depth.axis = FALSE)
aqp::plotSPC(subset(spc, spc$siteID == 'HARV'), color = 'phH2o', name.style = 'center-center', hz.depths = TRUE, plot.depth.axis = FALSE)
dev.off()# will reset par to default.

# prepare for dice()
#fillHzGaps Find and Fill Horizon Gaps
spc_fill <- aqp::fillHzGaps(spc, flag = TRUE)
# aggregate over profiles, 0-30
spc30cm <-  aqp::dice(spc_fill, fm= 0:30 ~ estimatedOC+carbonTot)
class(spc30cm)
spc30cm

#library(Hmisc)#for wtd.mean()
# compute hz-thickness weighted mean exchangeable-Ca:Mg
wt.mean.estimatedOC.percent <- function(i) {
  # use horizon thickness as a weight
  thick <- i$biogeoBottomDepth - i$biogeoTopDepth
  # compute the weighted mean, accounting for the possibility of missing data
  m <- Hmisc::wtd.mean(i$estimatedOC, weights=thick, na.rm=TRUE)*0.1 #unit: gramsPerKilogram g/kg = 0.1 %
  return(m)
}

# apply our custom function and save results as a site-level attribute
# results are 0-30cm weight mean OC in percent.
spc30cm$wtmean.estimatedOC.percent <- aqp::profileApply(spc30cm, wt.mean.estimatedOC.percent)

#unit: gramsPerKilogram g/kg = 0.1 %
spc30cm #715
# plot sliced data
par(mar = c(0,0,3,0)) # tighten figure margins
aqp::plotSPC(spc30cm[140:150,])
dev.off()

#spc 0-30 OC content calculation done-spc30cm_df:-------
# we get dataframe for spc 0-30 weighted mean OC in percent.
spc30cm_df <- aqp::site(spc30cm)# extract site data to data.frame

# }

# join:
# spc30cm_df
# spc_pits_spatial
# spc location and OC percent done! select columns later.---------
spc30cm_locationSOC <- left_join(spc30cm_df,spc_pits_spatial,by=c("domainID","siteID","plotID")) %>% 
  filter(!is.na(piteasting))


#start sls (soil periodic) location and SOC :--------
#plot location: plot centroid
sls_spatial <- read.csv(paste0(dir, outputs, "/spatial/spatial_sls.csv"), 
                        header = T, na.strings = "", stringsAsFactors = F)
#for organicCPercent:
sls_soilChem <- read.csv(paste0(dir, portalFolder, "/filesToStack10086/stackedFiles/sls_soilChemistry.csv"), 
                         header = T, na.strings = "", stringsAsFactors = F)
#for location of sampling
sls_CoreCollect <- read.csv(paste0(dir, portalFolder, "/filesToStack10086/stackedFiles/sls_soilCoreCollection.csv"), 
                            header = T, na.strings = "", stringsAsFactors = F)

#The getLocTOS() function requires two inputs:
#   A data table that contains spatial data from a NEON TOS data product
# The NEON table name of that data table

sls.loc.file=paste0(user_wd,"/NEON-AOP/data/hs_lai/NEON_SOC_Locations/sls.loc.csv")
if(file.exists(sls.loc.file)) {
  sls.loc <- read.csv(sls.loc.file)
}else{#download using getLocTOS
  # calculate individual  locations
  sls.loc <- getLocTOS(data=sls_CoreCollect,
                       dataProd="sls_soilCoreCollection",
                       token = NEON_TOKEN)
  class(sls.loc)
  head(sls.loc)
  write_csv(sls.loc,paste0(user_wd,"/NEON-AOP/data/hs_lai/NEON_SOC_Locations/sls.loc.csv"))
}

dim(sls_soilChem)#updated as 4183 from #3242  
sls_soilChem_short <- sls_soilChem %>% 
  filter(!is.na(organicCPercent)) %>%  # 806 row organicCPercent is NA,remove them
  select(namedLocation,domainID,siteID, plotID,sampleID,collectDate,
         organicCPercent) %>% 
    group_by(namedLocation,domainID,siteID, plotID,sampleID) %>% 
  summarise(OC_percent=mean(organicCPercent),
            collectDate=first(collectDate),
            n=n()) %>% ungroup
dim(sls_soilChem_short) #2937    8

sls.loc_short <- sls.loc %>% 
  select(namedLocation,domainID,siteID, plotID,sampleID,
         nlcdClass,
         collectDate,sampleTiming,horizon,sampleTopDepth,sampleBottomDepth,
         utmZone,adjNorthing,adjEasting)

sls30cm_locationSOC <- left_join(sls_soilChem_short,sls.loc_short,
                                 by=c( "namedLocation","domainID","siteID","plotID",
                                      "sampleID","collectDate")) %>% 
  filter(!is.na(OC_percent)) #update to 2937 from  #2282   16
dim(sls30cm_locationSOC)
sls30cm_locationSOC$collectDate <- substr(sls30cm_locationSOC$collectDate, 1, 10)
sls30cm_locationSOC$idcolumn <- sls30cm_locationSOC$sampleID
# remove O and M to make profile id
sls30cm_locationSOC <- sls30cm_locationSOC %>%
  mutate(across('idcolumn', str_replace,  "-[OM]-", "-"))

#some data are same profile but collectDate wrong, not in same day.
#some data have same idcolumn but slightly different collectDate, correct them.:
sls30cmlocation_df <- sls30cm_locationSOC %>% 
  group_by(idcolumn) %>% 
  mutate(collectDate=first(collectDate)) %>% 
  ungroup #update to 2468 #1910 unique(sls30cmlocation_df$idcolumn)

#copy it as sls to convert to soilprofile
sls=sls30cmlocation_df
# set up soil collections
aqp::depths(sls) <- idcolumn ~ sampleTopDepth + sampleBottomDepth
aqp::hzdesgnname(sls) <- 'horizon'
aqp::site(sls) <- ~ domainID + siteID + plotID +collectDate+namedLocation+nlcdClass+sampleTiming+
  utmZone+adjNorthing+adjEasting

#http://ncss-tech.github.io/AQP/aqp/dealing-with-bad-data.html
bad.chk <- aqp::checkHzDepthLogic(sls)
# highlight the problem--
# there are some overlap soil profile
idx <- which(bad.chk$overlapOrGap)
overlaphz <- aqp::horizons(sls[idx, ])
siteinfo=aqp::site(sls)
overlaphz$idcolumn
# site(sls)

sls_df <- aqp::horizons(sls)
nooverolap <- sls_df[!sls_df$idcolumn%in%overlaphz$idcolumn,]
fixoverlap <- sls_df[sls_df$idcolumn%in%overlaphz$idcolumn,]

n <- length(fixoverlap$sampleTopDepth)
fixoverlap$sampleBottomDepth[seq(n) %% 2 == 1]=fixoverlap$sampleTopDepth[seq(n) %% 2 == 0]
# overlap fixed, just make the M horizon top = O horizon bottom
sls_df <- bind_rows(nooverolap,fixoverlap)
sls_df=subset(sls_df, select = -c(hzID) )
#add site information, location, sites names...
sls_df <- left_join(sls_df,siteinfo,by="idcolumn")

sls=sls_df
# make depth as interger, because dice can not work at 0.1 digits
sls$sampleTopDepth=ceiling(sls$sampleTopDepth)
sls$sampleBottomDepth=ceiling(sls$sampleBottomDepth)

aqp::depths(sls) <- idcolumn ~ sampleTopDepth + sampleBottomDepth
aqp::hzdesgnname(sls) <- 'horizon'
aqp::site(sls) <- ~ domainID + siteID + plotID +collectDate+namedLocation+nlcdClass+sampleTiming+
  utmZone+adjNorthing+adjEasting
bad.chk <- aqp::checkHzDepthLogic(sls)
# highlight the problem--
# there are some overlap soil profile
idx <- which(bad.chk$overlapOrGap) 
length(idx) #0, no bad, no overlap

# prepare for dice()
z <- aqp::fillHzGaps(sls, to_top = 0, to_bottom = 30, flag = TRUE)
# aggregate over profiles, 0-30
sls30cm <-  aqp::dice(z, fm= 0:30 ~ OC_percent)
class(sls30cm)
sls30cm

#library(Hmisc)#for wtd.mean()
# compute hz-thickness weighted mean OC_percent
wt.mean.estimatedOC.percent <- function(i) {
  # use horizon thickness as a weight
  thick <- i$biogeoBottomDepth - i$biogeoTopDepth
  # compute the weighted mean, accounting for the possibility of missing data
  m <- Hmisc::wtd.mean(i$OC_percent, weights=thick, na.rm=TRUE) #unit: percent
  return(m)
}

# apply our custom function and save results as a site-level attribute
# results are 0-30cm weight mean OC in percent.
sls30cm$wtmean.estimatedOC.percent <- aqp::profileApply(sls30cm, wt.mean.estimatedOC.percent)

sls30cm #update to 2468 from #1910
# plot sliced data
par(mar = c(0,0,3,0)) # tighten figure margins
aqp::plotSPC(sls30cm[1:15,])
dev.off()

#sls 0-30 OC content calculation done-spc30cm_df:-------
# sls location and OC percent done! select columns later.---------
# we get dataframe for sls 0-30 weighted mean OC in percent.
sls30cm_locationSOC <- aqp::site(sls30cm)# extract site data to data.frame #2468   12

#start mpg location and SOC start:--------
## Chem
mgp_perbiogeo <- read.csv(paste0
                          (dir, portalFolder, "/filesToStack00096/stackedFiles/mgp_perbiogeosample.csv"),
                          header = T, na.strings = "", stringsAsFactors = F) %>% 
  select(domainID,siteID,pitNamedLocation,horizonID,horizonName,collectDate,estimatedOC,
         nitrogenTot,biogeoTopDepth,biogeoBottomDepth)%>% 
  filter(!is.na(estimatedOC)) %>%  
  filter(!is.na(biogeoBottomDepth)) %>% 
  group_by(domainID,siteID,pitNamedLocation,collectDate,
           biogeoTopDepth,biogeoBottomDepth) %>% 
  summarise(horizonID=first(horizonID),
            horizonName=first(horizonName),
            estimatedOC=mean(estimatedOC,na.rm = TRUE),
            nitrogenTot=mean(nitrogenTot,na.rm = TRUE),
                            n=n()) %>% ungroup %>% 
  filter(!is.na(estimatedOC)) 
  
#warning: site "PUUM" doesn't have lat and long. is na.
mgp_spatial <- read.csv(paste0(dir, outputs, "/spatial/spatial_mgp.csv"), 
                        header = T, na.strings = "", stringsAsFactors = F) %>% 
  select(namedLocation,locationType,domainID,siteID,
         decimalLatitude,decimalLongitude) %>% 
  type_convert %>% 
  dplyr::filter(!is.na(decimalLongitude)) %>% 
  filter(siteID!="ONAQ") #ONAQ decimalLongitude is wrong, checked with eyes!
#47 sites total, remove "PUUM" and "ONAQ", 45 left

mgp_locationSOC <- left_join(mgp_perbiogeo,mgp_spatial,by=c("domainID","siteID",
                                                            "pitNamedLocation"="namedLocation"))%>% 
  filter(!is.na(estimatedOC)) %>% 
  filter(!is.na(decimalLongitude))

mgp=mgp_locationSOC 
  
aqp::depths(mgp) <- pitNamedLocation ~ biogeoTopDepth + biogeoBottomDepth
aqp::hzdesgnname(mgp) <- 'horizonName'
aqp::site(mgp) <- ~ domainID + siteID +collectDate+decimalLatitude+decimalLongitude
bad.chk <- aqp::checkHzDepthLogic(mgp)
# highlight the problem--  MEGAPT100982; MEGAPT100964
# there are some overlap soil profile
idx <- which(bad.chk$overlapOrGap) 
length(idx) #2 #0, no bad, no overlap

# prepare for dice()
z <- aqp::fillHzGaps(mgp, to_top = 0, to_bottom = 30, flag = TRUE)
bad.chk <- aqp::checkHzDepthLogic(z)
idx <- which(bad.chk$overlapOrGap) 
length(idx) #0, no bad, no overlap
# aggregate over profiles, 0-30
mgp30cm <-  aqp::dice(z, fm= 0:30 ~ estimatedOC)
class(mgp30cm)
mgp30cm

#library(Hmisc)#for wtd.mean()
# compute hz-thickness weighted mean exchangeable-Ca:Mg
wt.mean.estimatedOC.percent <- function(i) {
  # use horizon thickness as a weight
  thick <- i$biogeoBottomDepth - i$biogeoTopDepth
  # compute the weighted mean, accounting for the possibility of missing data
  m <- Hmisc::wtd.mean(i$estimatedOC, weights=thick, na.rm=TRUE)*0.1 #unit: gramsPerKilogram g/kg = 0.1 %
  return(m)
}

# apply our custom function and save results as a site-level attribute
# results are 0-30cm weight mean OC in percent.
mgp30cm$wtmean.estimatedOC.percent <- aqp::profileApply(mgp30cm, wt.mean.estimatedOC.percent)
#unit: gramsPerKilogram g/kg = 0.1 %
mgp30cm #45
# plot sliced data
par(mar = c(0,0,3,0)) # tighten figure margins
aqp::plotSPC(mgp30cm[28:35,])
dev.off()

#mgp 0-30 OC content calculation done-spc30cm_df:-------
# we get dataframe for mgp 0-30 weighted mean OC in percent.
mgp30cm_df <- aqp::site(mgp30cm)# extract site data to data.frame
#covert megapits lat long to UTM--------
#first need to find the UTM zone for each site
#each site has one magapit
library(rgdal)
NEON_plots <- readOGR(dsn = paste0(user_wd,"/NEON-AOP/data/All_NEON_TOS_Plots_V8"),
                      layer = "All_NEON_TOS_Plot_Polygons_V8")
tower_plots <- NEON_plots@data %>%
  filter(plotType == "tower" & subtype == "basePlot") %>%
  arrange(plotID) %>% 
  select(domain,domainID,siteID,plotType,
         latitude,longitude,utmZone,easting,northing) %>% 
  group_by(domain,domainID,siteID,plotType) %>% 
  summarise(utmZone=unique(utmZone))
  
mgp30cm_df_utm <- left_join(mgp30cm_df,tower_plots,by=c("domainID","siteID" )) %>% 
  mutate(east=decimalLongitude,
         north=decimalLatitude)

# install.packages("PBSmapping")
library("PBSmapping")
for (i in 1:nrow(mgp30cm_df_utm)) {
  latlong=mgp30cm_df_utm[i,] %>%
    select(X=decimalLongitude,Y=decimalLatitude)
  attr(latlong, "projection") <- "LL"
  utmzone= as.numeric(gsub("N", "", mgp30cm_df_utm[i,10]))
  attr(latlong, "zone") <- utmzone 
  latlongUTM <- convUL(latlong, km=FALSE)
  mgp30cm_df_utm[i,11]=latlongUTM$X
  mgp30cm_df_utm[i,12]=latlongUTM$Y
}
# megapits location and OC percent done! select columns later.---------
mgp30cm_locationSOC=mgp30cm_df_utm


#save three dataframe, don't need to run another time
#sls has some data are same location but different profile ID, some has only organic layer, 
# cleaned after combine three data frame
write_csv(spc30cm_locationSOC,paste0(user_wd,"/NEON-AOP/data/hs_lai/NEON_SOC_Locations/spc30cm_locationSOC.csv"))
write_csv(sls30cm_locationSOC,paste0(user_wd,"/NEON-AOP/data/hs_lai/NEON_SOC_Locations/sls30cm_locationSOC.csv"))
write_csv(mgp30cm_locationSOC,paste0(user_wd,"/NEON-AOP/data/hs_lai/NEON_SOC_Locations/mgp30cm_locationSOC.csv"))

#combine three as one csv
temp_spc=spc30cm_locationSOC %>% 
  select(domainID,siteID,collectDate,wtmean.estimatedOC.percent,
         utmZone,piteasting,pitnorthing)

temp_sls=sls30cm_locationSOC%>% 
  select(domainID,siteID,collectDate,wtmean.estimatedOC.percent,
         utmZone,piteasting=adjEasting,pitnorthing=adjNorthing)

temp_mgp=mgp30cm_locationSOC %>% 
  select(domainID,siteID,collectDate,wtmean.estimatedOC.percent,
         utmZone,piteasting=east,pitnorthing=north)

combine <- bind_rows(temp_spc,temp_mgp,temp_sls) %>% 
  filter(!is.na(wtmean.estimatedOC.percent))

#sls has some data are same location but different profile ID, some has only organic layer------
location=combine[ , c("piteasting", "pitnorthing")]
dim(combine)#2669    7
dim(unique(location))#2656    2 some data has same location
dup=location[duplicated(location),]
i=1
combine[combine$piteasting==dup[i,1]&combine$pitnorthing==dup[i,2],]

combine_clean=combine %>% group_by(domainID,siteID,utmZone,piteasting,pitnorthing) %>%
  mutate(wtmean.estimatedOC.percent=mean(wtmean.estimatedOC.percent,na.rm = TRUE)) %>% 
  slice_head() %>% ungroup()

combine_clean[combine_clean$piteasting==dup[i,1]&combine_clean$pitnorthing==dup[i,2],]
#FINAL products sls,spc,mgp 30cm soil SOC and location in UTM-----
write_csv(combine_clean,paste0(user_wd,"/NEON-AOP/data/hs_lai/NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC.csv"))





