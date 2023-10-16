##########################################################################
### CODE FOR DOWNLOADING DATA FROM NEON DATA PORTAL
### Created Feb 2018 for SoDaH project
### Updated September 2021 for Sihi/Weintraub-Leff MSB project
### Updataed Apr 2023 for AOP project，sls updated on 2022,2023
### S. Weintraub-Leff; zhuonan wang
##########################################################################

### Reset workspace
rm(list = ls())

### Load packages - CRAN
library(tidyverse) # joining and wrangling functions
library(neonUtilities) # to download NEON data

### Load packages - Git
# library(devtools)
# geoNEON - for getting spatial data about sampling sites. uncomment and run line below if need this package
# install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=T) 
library(geoNEON)
NEON_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ6aHVvbmFuLndhbmdAZW1vcnkuZWR1Iiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxNzg3Njk4ODU4LCJpYXQiOjE2MzAwMTg4NTgsImVtYWlsIjoiemh1b25hbi53YW5nQGVtb3J5LmVkdSJ9.Ug3DuSLWLT0NsZy5pHKs8Wh51C9-tjxoCccbhrUW0m49n1GikPp0EQ3jZc3c6FFzRpDk04UXA-k4fECyxzqnmQ"

### Set directories - new users add your own path as needed
# If code has been run before, make a new folder with the date to put data files into, or delete/archive old outputs
# Code won't run if old folders present in the destination with same names

#setwd to home, then check which pc.
setwd("~")
whichpc=getwd()
cat(whichpc)
if(whichpc=="/Users/zwang61"){
  user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
}else{
  user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
}

  dir <- file.path(user_wd,"NEON-AOP/data/neon_data") 
  portalFolder <- "/portal"
  outputs <- "/compiled"

  # If file sizes are large, increase the timeout limit on your machine: 
  options(timeout=60*120)
  getOption('timeout')
  
### Code to download NEON Data (soil C + covariates)
## Token is optional, makes download faster but can remove this argument if don't have 1

# Soil physical and chemical properties, Megapit, DP1.00096.001
{
  zipsByProduct(dpID="DP1.00096.001", site="all", package="basic", check.size=F, 
                savepath = paste0(dir, portalFolder), token = NEON_TOKEN) +
  stackByTable(paste0(dir, portalFolder, "/filesToStack00096"), folder=T, saveUnzippedFiles = F)
}

# Soil physical and chemical properties, Distributed initial characterization, DP1.10047.001
{
  zipsByProduct(dpID="DP1.10047.001", site="all", package="basic", check.size=F,
                savepath = paste0(dir, portalFolder), token = NEON_TOKEN) +
    stackByTable(paste0(dir, portalFolder, "/filesToStack10047"), folder=T, saveUnzippedFiles = F)
}
# Soil physical and chemical properties, distributed periodic, DP1.10086.001 - large, takes a while
{
  zipsByProduct(dpID="DP1.10086.001", site="all", package="basic", check.size=F,
                savepath = paste0(dir, portalFolder), token = NEON_TOKEN) +
    stackByTable(paste0(dir, portalFolder, "/filesToStack10086"), folder=T, saveUnzippedFiles = F)
}

### Get physiographic data about the plots (slope, aspect, etc) using geoNEON package
## Entire code section can take 20-30 minutes to run

{
# read in one of the soil megapit files
mgp_permegapit <- read.csv(paste0
                          (dir, portalFolder, "/filesToStack00096/stackedFiles/mgp_permegapit.csv"), 
                          header = T)
# one of the soil initial characterization files
spc_perplot <- read.csv(paste0
                        (dir, portalFolder, "/filesToStack10047/stackedFiles/spc_perplot.csv"),
                        header = T)
# one of the soil periodic plot files
sls_CoreCollect <- read.csv(paste0
                        (dir, portalFolder, "/filesToStack10086/stackedFiles/sls_soilCoreCollection.csv"),
                        header = T)


# use getLocByName function in geoNEON to get plot-level metadata
mgp <- getLocByName(mgp_permegapit, 'pitNamedLocation', locOnly=T)
spc <- getLocByName(spc_perplot, 'namedLocation', locOnly=T)
# for periodic distributed plots (sls), make a smaller df with just locations first
slsLocations <- as.data.frame(unique(sls_CoreCollect$namedLocation))
colnames(slsLocations) <- "namedLocation"
sls <- getLocByName(slsLocations, 'namedLocation', locOnly=T)

# save files
write.csv(mgp, paste(dir, "compiled/spatial/spatial_mgp.csv", sep = "/"), row.names = F)
write.csv(spc, paste(dir, "compiled/spatial/spatial_spc.csv", sep = "/"), row.names = F)
write.csv(sls, paste(dir, "compiled/spatial/spatial_sls.csv", sep = "/"), row.names = F)

}
