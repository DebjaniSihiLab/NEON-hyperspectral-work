# download NEON-AOP flight line data DP1
library(neonUtilities)
#byFileAOP
# If file sizes are large, increase the timeout limit on your machine: 
options(timeout=60*120)
getOption('timeout')
NEON_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ6aHVvbmFuLndhbmdAZW1vcnkuZWR1Iiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxNzg3Njk4ODU4LCJpYXQiOjE2MzAwMTg4NTgsImVtYWlsIjoiemh1b25hbi53YW5nQGVtb3J5LmVkdSJ9.Ug3DuSLWLT0NsZy5pHKs8Wh51C9-tjxoCccbhrUW0m49n1GikPp0EQ3jZc3c6FFzRpDk04UXA-k4fECyxzqnmQ"
library(geoNEON)
library(neonhs)
library(rhdf5)
library(tidyverse)
library(terra)
library(raster)
library(rgdal)
library(sf)

{#domain 10 testing:-----
  #setwd to home, then check which pc.
  setwd("~")
  whichpc=getwd()
  cat(whichpc)
  if(whichpc=="/Users/zwang61"){
    user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }else{
    user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  } 
    
  spc_mgp_sls_30cm_locationSOC <- read_csv(paste0(user_wd,"NEON-AOP/data/hs_lai/NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC.csv"))
  spc_mgp_sls_30cm_locationSOC$rowID=1:nrow(spc_mgp_sls_30cm_locationSOC)
  allsites=unique(spc_mgp_sls_30cm_locationSOC$siteID)
  length(allsites) #47 we have all NEON has 47 terrestrial sites
  allsites
    
  domainID_10 <- spc_mgp_sls_30cm_locationSOC %>% 
    filter(domainID=="D10")
  domain10_sites=unique(domainID_10$siteID)
  domain10_sites
}

#run function:
download_HS_flightlines(spc_mgp_sls_30cm_locationSOC,site="RMNP",
                        downloadAOPfile=TRUE,year=2021)

#----------------------------------------------------------------
#define fucntion download_HS_flightlines() # NEON-AOP flight line data DP1
#single site,
#year=0 then use latest available year download;
#year=2019 then use 2019 year download
download_HS_flightlines <- function(spc_mgp_sls_30cm_locationSOC,
                                           site="CPER",
                                           downloadAOPfile=FALSE,
                                           year=2021){
#setwd to home, then check which pc.
  setwd("~")
  whichpc=getwd()
  cat(whichpc)
  if(whichpc=="/Users/zwang61"){
      user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
    }else{
      user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
    }
  #wd set to my external hard drive-seagate----
  # we want to save our files. Be sure to move the download into your working directory!
  wd="/Volumes/seagate/"
  setwd(wd)
    # wd=paste0(user_wd,"NEON-AOP/data/hs_lai")
    # setwd(wd)

cat("DP1.30006.001---> download NEON-AOP flight line data DP1, hyperspectral data: \n")  
    
    
HARV_spc <-  spc_mgp_sls_30cm_locationSOC %>% filter(siteID == site) %>%
    arrange(rowID)
cat(site," site: \n")  
  #We want the soil plot hyperspectral data, all plots have soil(spc) measurements
  # cat(HARV_spc$utmZone)
  east <- HARV_spc$piteasting
  names(east) <- HARV_spc$rowID
  north <- HARV_spc$pitnorthing
  names(north) <- HARV_spc$rowID
  # cat(paste0("east: ",east,"\n"))
  # cat(paste0("north: ",east,"\n"))
  unique(HARV_spc$utmZone)
  cat(paste0("This site SOC location numbers: ",length(east))," \n")
  
  #year, which year is the best, hwo to decide the year
  # query the products endpoint for the product requested
  source(paste0(user_wd,"/NEON-AOP/code/My_functions_R/getAPI.R"))
  prod.req <- getAPI(apiURL = paste("http://data.neonscience.org/api/v0/products/", "DP1.30006.001", sep=""), 
                     token = NEON_TOKEN)
  avail <- jsonlite::fromJSON(httr::content(prod.req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)

  # get the urls for months with data available, and subset to site
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  month.urls <- grep(site, month.urls, value = TRUE)
  
  avilb_year <- str_split(month.urls, "/") %>% 
    map_vec(9) %>% 
    substr(1,4) %>% 
    as.numeric()
  
  latest_year <- max(avilb_year)
  cat("\n",paste0("site:",site,"\navailable AOP flight line data in year: "),avilb_year,
      "\nlatest year: ",latest_year,"\n")
  if(year==0){
    cat("didn't specify year, use  available latest_year","\n")
  }else{
    if(!year %in% avilb_year){
      cat("user choose year is not in available year at this site.----->\n",
          "use  available latest_year",latest_year,"\n\n")
    }else{
      latest_year <- year
      cat("download user choose year, latest_year=", year,"\n")
    }
  }
  
  # Download hyperspectral imagery from NEON-AOP-----------------------------
  
  # the Hyperspectral Remote Sensing Data in HDF5 Format for that site.
  
  #make sure there is no this sites HDF5 files
  dir <- paste0(wd,"/DP3.30006.001/neon-aop-products/",latest_year,"/FullSite/",unique(HARV_spc$domainID))
  dirs <- list.dirs(dir)
  sites_dirs <- grep(paste0(latest_year,"_",site), dirs)
  length(sites_dirs)#if there is no this sites file, length==0,then download them.
  
  if (length(sites_dirs)==0){
    cat("\n","this site HDF5 data doesn't exists\n")
  }else{
    cat("\n","this site HDF5 data exists\n")
  }
  
  cat("Parameter downloadAOPfile =", downloadAOPfile,"\n")
  
  if(downloadAOPfile==TRUE){
    cat("download this site SOC plots HDF5 Tiles \n")
    options(timeout=60*120)
    getOption('timeout')
  #set  check.size = F, so continously download
  #HS download site-year by flight line
  byFileAOP(dpID = "DP1.30006.001", # NEON-AOP flight line data DP1
              site = site, # Site code
              year = latest_year, # Year
              check.size = F,
              savepath = wd, # Path
              token = NEON_TOKEN)
  }else{
    cat("\n","do not allow to download this site HDF5 data\n")
  }
  
}
 
  