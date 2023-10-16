# Load `raster` and `rhdf5` packages and read NIS data into R
library(raster)
library(rhdf5)
library(rgdal)
library(tidyverse)
NEON_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ6aHVvbmFuLndhbmdAZW1vcnkuZWR1Iiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxNzg3Njk4ODU4LCJpYXQiOjE2MzAwMTg4NTgsImVtYWlsIjoiemh1b25hbi53YW5nQGVtb3J5LmVkdSJ9.Ug3DuSLWLT0NsZy5pHKs8Wh51C9-tjxoCccbhrUW0m49n1GikPp0EQ3jZc3c6FFzRpDk04UXA-k4fECyxzqnmQ"


hdf5_cloud_condition <- function(site="TOOL",year=2021){
  user_wd="/lustre/or-scratch/cades-ccsi/scratch/z2w/NEON_hs/"
  wd="/lustre/or-scratch/cades-ccsi/scratch/z2w/NEON_hs/data/"
  setwd(wd)
  load(file='shared_flights.rda')
  shared_flights <- shared_flights[-2,]
  
  if(site %in% shared_flights$site){
    flightsite=shared_flights$flightSite[which(site==shared_flights$site)]
  }else{
    flightsite=site
  }
  # query the products endpoint for the product requested
  source(paste0(user_wd,"/code/My_functions_R/getAPI.R"))
  prod.req <- getAPI(apiURL = paste("http://data.neonscience.org/api/v0/products/", "DP1.30006.001", sep=""), 
                     token = NEON_TOKEN)
  avail <- jsonlite::fromJSON(httr::content(prod.req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)
  
  # get the urls for months with data available, and subset to site
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  month.urls <- grep(flightsite, month.urls, value = TRUE)
  
  avilb_year <- str_split(month.urls, "/") %>% 
    map(9) %>% 
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
  
  if(site=="CHEQ"){
    latest_year <- year
  }
  
  dir <- paste0(wd,"/DP1.30006.001/neon-aop-products/",latest_year,"/FullSite/")
  dirs <- list.dirs(dir)
  sites_dirs <- grep(paste0(latest_year,"_",flightsite), dirs,value =TRUE)
  files_corrected=list.files(sites_dirs, pattern = "_reflectance.h5$",full.names = TRUE)
  
  out <- NULL
  for (i in seq_along(files_corrected)) {
    f <- files_corrected[i]
    # sitenames=h5ls(f)$name[1]
 
    datapath <- paste0("/",flightsite, '/Reflectance/Reflectance_Data')
    
    reflInfo <- h5readAttributes(f,datapath)
    # reflInfo
    # reflInfo$`Cloud conditions`
    
    flightnames <- basename(f)
    oneflightline <- data.frame(names=flightnames,cloud=reflInfo$`Cloud conditions`)
    out=rbind(out,oneflightline)
  }
  
  write_csv(out, 
            paste0(wd,"DP1.30006.001/cloud/",
                   site,"_",latest_year,".csv")
  )
  
  
}
  
#=================================
user_wd="/lustre/or-scratch/cades-ccsi/scratch/z2w/NEON_hs/"
spc_mgp_sls_30cm_locationSOC <- read_csv(paste0(user_wd,"spc_mgp_sls_30cm_locationSOC_withPFT_Soiltype.csv"))
spc_mgp_sls_30cm_locationSOC$rowID=1:nrow(spc_mgp_sls_30cm_locationSOC)
allsites=unique(spc_mgp_sls_30cm_locationSOC$siteID)
length(allsites) #47 we have all NEON has 47 terrestrial sites + CHEQ total 48
allsites
validsites <- allsites[!allsites%in% c("LAJA","ORNL")]
neonsites <- read_csv(paste0(user_wd,"neon_hs_sites.csv"))
neonsites$site %>% length()

for(a_site in validsites){
  siteyear=neonsites[neonsites$site==a_site,2][[1]]
  #run function:
  hdf5_cloud_condition(site=a_site,year=siteyear)
}




