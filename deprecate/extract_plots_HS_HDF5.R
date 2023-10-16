#Extract hyperspectral data at site including all plots center with windows 3m*3m  

#install.packages('devtools')
# devtools::install_github('earthlab/neonhs')
NEON_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ6aHVvbmFuLndhbmdAZW1vcnkuZWR1Iiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxNzg3Njk4ODU4LCJpYXQiOjE2MzAwMTg4NTgsImVtYWlsIjoiemh1b25hbi53YW5nQGVtb3J5LmVkdSJ9.Ug3DuSLWLT0NsZy5pHKs8Wh51C9-tjxoCccbhrUW0m49n1GikPp0EQ3jZc3c6FFzRpDk04UXA-k4fECyxzqnmQ"
library(geoNEON)
library(neonhs)
library(rhdf5)
library(tidyverse)
library(terra)
library(raster)
library(rgdal)
library(sf)
#____________________________
#start to extract each site----
#____________________________
{
  #setwd to home, then check which pc
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
}
#use latest available year.

#1. escape c("BLAN",'STEI','TREE','CHEQ','KONA','DCFS')--------
#define function extract_HS_with_buffer()-------

extract_HS_with_buffer <- function(spc_mgp_sls_30cm_locationSOC,
                                   site="HARV",
                                   year=2021,
                                   buffer=3){
  #setwd to home, then check which pc.
  setwd("~")
  whichpc=getwd()
  cat(whichpc)
  if(whichpc=="/Users/zwang61"){
    user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }else{
    user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }
  
  wd=paste0(user_wd,"NEON-AOP/data/hs_lai/")
  setwd(wd)
  
  HARV_spc <-  spc_mgp_sls_30cm_locationSOC %>% filter(siteID == site) %>%
    arrange(rowID)
  #View(HARV_spc)
  #We want the soil plot hyperspectral data, all plots have soil(spc) measurements
  # cat(HARV_spc$utmZone)
  east <- HARV_spc$piteasting
  names(east) <- HARV_spc$rowID
  north <- HARV_spc$pitnorthing
  names(north) <- HARV_spc$rowID
  # cat(paste0("east: ",east,"\n"))
  # cat(paste0("north: ",east,"\n"))
  utm <- unique(HARV_spc$utmZone)
  cat(utm,"\n")
  if(length(utm)!=1){
    cat("This site ",site, " has more than one UTM zone! stop \n")
    cat(site,file="outfile_wrongUTM_files.txt",sep="\n",append=TRUE)
  }
  site_crs <- paste0("+proj=utm +zone=",parse_number(utm))
  
  
  # query the products endpoint for the product requested
  source(paste0(user_wd,"/NEON-AOP/code/My_functions/getAPI.R"))
  prod.req <- getAPI(apiURL = paste("http://data.neonscience.org/api/v0/products/", "DP3.30006.001", sep=""), 
                     token = NEON_TOKEN)
  avail <- jsonlite::fromJSON(httr::content(prod.req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)
  # error message if product not found
  if(!is.null(avail$error$status)) {
    stop(paste("No data found for product","DP3.30006.001", sep=" "))
  }
  
  # get the urls for months with data available, and subset to site
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  month.urls <- grep(site, month.urls, value = TRUE)
  
  avilb_year <- str_split(month.urls, "/") %>% 
    map_vec(9) %>% 
    substr(1,4) %>% 
    as.numeric()
  
  latest_year <- max(avilb_year)
  cat("\n",paste0("site:",site,"\navailable AOP data in year: "),avilb_year,
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
  
  dirs <- list.dirs(paste0(wd,'/DP3.30006.001'),recursive = TRUE) 
  dirs <- grep(paste0(latest_year,"_",site,".*Reflectance"), dirs, value = TRUE)
  hs_paths <- list.files(path=dirs, 
                         pattern = paste0(".*",site,".*reflectance.h5"), 
                         recursive = TRUE, full.names = TRUE)
  
  #check there is error:HDF5  Can't open object-
  extents <- list()
  for(i in 1:length(hs_paths)){
    test_open_hs <- try(hs_extent(hs_paths[i]))
    if(inherits(test_open_hs, "try-error")){
      cat("cannot open this HDF5. \n")
      cat(hs_paths[i],file="outfile_wrongHDF5_files.txt",sep="\n",append=TRUE)
    }else{
      if(proj4string(CRS(hs_proj4string(hs_paths[i])))==proj4string(CRS(site_crs))){
        extents[[i]] <- test_open_hs
      }else{
        cat("crs have problem. \n")
        cat(hs_paths[i],file="outfile_wrongCRS_files.txt",sep="\n",append=TRUE)
      }
      
    }
  }
  
  east_addbuffer <- NULL
  north_addbuffer <- NULL
  if(buffer>0){
    for(k in 1:length(east)) {
      bufferEasting <- c(east[k]+1:buffer,east[k],east[k]-1:buffer)
      # names(bufferEasting) <- rep(names(east[k]),buffer+buffer+1)
      bufferNorthing <- c(north[k]+1:buffer,north[k],north[k]-1:buffer)
      # names(bufferNorthing) <- rep(names(north[k]),buffer+buffer+1)                
      buffer_pixels <- expand_grid(bufferEasting,bufferNorthing)        
      
      east_windows <- buffer_pixels$bufferEasting
      names(east_windows) <- rep(names(east[k]),length(east_windows))
      north_windows <- buffer_pixels$bufferNorthing
      names(north_windows) <- rep(names(north[k]),length(north_windows))
      
      east_addbuffer <- c(east_addbuffer,east_windows )
      north_addbuffer <- c(north_addbuffer,north_windows )
    }
  }
  
  #Now, create a SpatialPointsDataFrame of these points.
  spdf_buffer <- SpatialPointsDataFrame(coords = data.frame(x = c(east_addbuffer),
                                                            y = c(north_addbuffer)), 
                                        data = data.frame(plotID = names(east_addbuffer)),
                                        proj4string = sp::CRS(site_crs))
  
  # We can use the neonhs package to extract hyperspectral data at these locations.
  out <- list()
  for (i in seq_along(hs_paths)) {
    res <- hs_extract_pts(hs_paths[i], pts = spdf_buffer, bands = 1:426)
    first_band <- grep('^band1', names(res), value = TRUE)[1]
    na_vals <- is.na(res[[first_band]])
    out[[i]] <- res[!na_vals, ]
  }
  
  # Now let’s create a tibble for easy plotting:
  hs_df <- lapply(out, as.data.frame) %>% 
    bind_rows %>% 
    as_tibble %>%
    dplyr::select(plotID, east=x,north=y, 
                  starts_with('band')) %>% 
    distinct
  hs_df   
  
  # hs_extract_pts_cloud() 
  #fuction to extract cloud contidion:cloud cover (green <10%, yellow 10%-50%, red >50%).
  source(paste0(user_wd,"/NEON-AOP/code/My_functions/hs_extract_pts_cloud.R"))
  out_cloud <- list()
  for (i in seq_along(hs_paths)) {
    res <- hs_extract_pts_cloud(hs_paths[i], pts = spdf_buffer, bands = 1:3)
    na_vals <- (res[[ "cloud_index"]]=="Fail")
    out_cloud[[i]] <- res[!na_vals, ]
  }
  
  #  create a tibble for easy plotting:
  cloud_df <- lapply(out_cloud, as.data.frame) %>% 
    bind_rows %>% 
    as_tibble %>%
    dplyr::select(plotID, east=x,north=y,cloud_index) 
  
  #check cloud condition, see if all is red
  not_red_row=nrow(cloud_df)
  cat("\n","Cloud conditions:------\n")
  cat("\n",paste0(latest_year,"-",site," cloud available plots ",not_red_row),"\n")
  cat("\n","Write cloud conditions:\n")
  write_csv(cloud_df, 
            paste0(wd,"DP3.30006.001/",latest_year,"_",unique(HARV_spc$domainID),"_",site,"_","cloud_df_windows",buffer,"m.csv"))
  cat("\n","=============\n")
  #remove cloud index not OK, green and yellow are OK, red is Fail
  hs_df_cldOK <- left_join(hs_df,cloud_df, by = c("plotID", "east","north")) %>% 
    filter(cloud_index=="OK")
  
  rm(out_cloud,out) #to save memory
  # And gather the hyperspectral columns (converting the data from wide to long form).
  long_df <- hs_df_cldOK %>%
    gather(band, reflectance, 
           -plotID,-east,-north,-cloud_index) %>%
    separate(band, c('index', 'wavelength')) %>%
    mutate(wavelength = parse_number(wavelength)) %>% 
    # filter water vapor bands out
    # filter(!between(wavelength, 1340, 1445),
    #        !between(wavelength, 1790, 1955)) %>% 
    mutate(reflectance = case_when((wavelength>1340&wavelength<1445) ~NA,
                                   (wavelength>1790&wavelength<1955)~NA,
                                   TRUE ~ reflectance)) %>% 
    group_by(plotID,cloud_index, index, wavelength) %>% 
    summarise(reflectance = mean(reflectance),
              n=n())
  long_df 
  
  write_csv(long_df, 
            paste0(wd,"DP3.30006.001/",latest_year,"_",unique(HARV_spc$domainID),"_",site,"_","HS_windows",buffer,"m.csv"))
  
  return(
    cat("\n",site,": HS succeed!-",paste0("\n","long_df saved:\n",
                                          paste0(wd,"DP3.30006.001/",latest_year,"_",
                                                 unique(HARV_spc$domainID),"_",site,"_","HS_windows",buffer,"m.csv")),
        "\n\n")
  )
  
}

records=NULL

processed_sites=allsites[!allsites %in% c("BLAN",'STEI','TREE','CHEQ','KONA','DCFS')]
#processed_sites=allsites[!allsites %in% c("BLAN",'STEI','TREE','CHEQ','KONA','DCFS')]-----
start <- Sys.time()
for (Onesite in processed_sites) {
  extract_HS_with_buffer(
    spc_mgp_sls_30cm_locationSOC,
    site= Onesite,
    year=0,
    buffer=3
  )
  records=cbind(records,Onesite)
}  

cat(records)
print( Sys.time() - start ) 

#2.processed_sites c('TREE','CHEQ','KONA','DCFS')-------
# check for sites that are flown under the flight box of a different site
site='TREE'
if(site %in% c('TREE','CHEQ','KONA','DCFS' )) {
  cat("sites that are flown under the flight box of a different site\n")
}

#define function extract_HS_with_buffer_shared_flights-----
extract_HS_with_buffer_shared_flights <- function(spc_mgp_sls_30cm_locationSOC,
                                   site="HARV",
                                   year=2021,
                                   buffer=3){
  #setwd to home, then check which pc.
  setwd("~")
  whichpc=getwd()
  cat(whichpc)
  if(whichpc=="/Users/zwang61"){
    user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }else{
    user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }
  
  wd=paste0(user_wd,"NEON-AOP/data/hs_lai/")
  setwd(wd)
  
  # check for sites that are flown under the flight box of a different site
  if(site %in% shared_flights$site) {
    flightSite <- shared_flights$flightSite[which(shared_flights$site==site)]
    if(site %in% c('TREE','CHEQ','KONA','DCFS')) {
      cat(paste(site, ' is part of the flight box for ', flightSite,
                '. Using data from ', flightSite, '.\n', sep=''))
    } else {
      cat(paste(site, ' is an aquatic site and is sometimes included in the flight box for ', flightSite,
                '. Aquatic sites are not always included in flight coverage every year.\nDownloading data from ',
                flightSite, '. Check data to confirm coverage of ', site, '.\n', sep=''))
    }
    originalsitename=site
    site <- flightSite
   
  }
  
  HARV_spc <-  spc_mgp_sls_30cm_locationSOC %>% filter(siteID == site) %>%
    arrange(rowID)
  #View(HARV_spc)
  #We want the soil plot hyperspectral data, all plots have soil(spc) measurements
  # cat(HARV_spc$utmZone)
  east <- HARV_spc$piteasting
  names(east) <- HARV_spc$rowID
  north <- HARV_spc$pitnorthing
  names(north) <- HARV_spc$rowID
  # cat(paste0("east: ",east,"\n"))
  # cat(paste0("north: ",east,"\n"))
  utm <- unique(HARV_spc$utmZone)
  cat(utm,"\n")
  if(length(utm)!=1){
    cat("This site ",site, " has more than one UTM zone! stop \n")
    cat(site,file="outfile_wrongUTM_files.txt",sep="\n",append=TRUE)
  }
  site_crs <- paste0("+proj=utm +zone=",parse_number(utm))
  
  
  # query the products endpoint for the product requested
  source(paste0(user_wd,"/NEON-AOP/code/My_functions/getAPI.R"))
  prod.req <- getAPI(apiURL = paste("http://data.neonscience.org/api/v0/products/", "DP3.30006.001", sep=""), 
                     token = NEON_TOKEN)
  avail <- jsonlite::fromJSON(httr::content(prod.req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)
  # error message if product not found
  if(!is.null(avail$error$status)) {
    stop(paste("No data found for product","DP3.30006.001", sep=" "))
  }
  
  # get the urls for months with data available, and subset to site
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  month.urls <- grep(site, month.urls, value = TRUE)
  
  avilb_year <- str_split(month.urls, "/") %>% 
    map_vec(9) %>% 
    substr(1,4) %>% 
    as.numeric()
  
  latest_year <- max(avilb_year)
  cat("\n",paste0("site:",site,"\navailable AOP data in year: "),avilb_year,
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
  
  dirs <- list.dirs(paste0(wd,'/DP3.30006.001'),recursive = TRUE) 
  dirs <- grep(paste0(latest_year,"_",site,".*Reflectance"), dirs, value = TRUE)
  hs_paths <- list.files(path=dirs, 
                         pattern = paste0(".*",site,".*reflectance.h5"), 
                         recursive = TRUE, full.names = TRUE)

  
  #check there is error:HDF5  Can't open object--
  extents <- list()
  for(i in 1:length(hs_paths)){
    test_open_hs <- try(hs_extent(hs_paths[i]))
    if(inherits(test_open_hs, "try-error")){
      cat("cannot open this HDF5. \n")
      cat(hs_paths[i],file="outfile_wrongHDF5_files.txt",sep="\n",append=TRUE)
    }else{
      if(proj4string(CRS(hs_proj4string(hs_paths[i])))==proj4string(CRS(site_crs))){
        extents[[i]] <- test_open_hs
      }else{
        cat("crs have problem. \n")
        cat(hs_paths[i],file="outfile_wrongCRS_files.txt",sep="\n",append=TRUE)
      }
      
    }
  }
  
  east_addbuffer <- NULL
  north_addbuffer <- NULL
  if(buffer>0){
    for(k in 1:length(east)) {
      bufferEasting <- c(east[k]+1:buffer,east[k],east[k]-1:buffer)
      # names(bufferEasting) <- rep(names(east[k]),buffer+buffer+1)
      bufferNorthing <- c(north[k]+1:buffer,north[k],north[k]-1:buffer)
      # names(bufferNorthing) <- rep(names(north[k]),buffer+buffer+1)                
      buffer_pixels <- expand_grid(bufferEasting,bufferNorthing)        
      
      east_windows <- buffer_pixels$bufferEasting
      names(east_windows) <- rep(names(east[k]),length(east_windows))
      north_windows <- buffer_pixels$bufferNorthing
      names(north_windows) <- rep(names(north[k]),length(north_windows))
      
      east_addbuffer <- c(east_addbuffer,east_windows )
      north_addbuffer <- c(north_addbuffer,north_windows )
    }
  }
  
  #Now, create a SpatialPointsDataFrame of these points.
  spdf_buffer <- SpatialPointsDataFrame(coords = data.frame(x = c(east_addbuffer),
                                                            y = c(north_addbuffer)), 
                                        data = data.frame(plotID = names(east_addbuffer)),
                                        proj4string = sp::CRS(site_crs))
  
  # We can use the neonhs package to extract hyperspectral data at these locations.
  out <- list()
  for (i in seq_along(hs_paths)) {
    res <- hs_extract_pts(hs_paths[i], pts = spdf_buffer, bands = 1:426)
    first_band <- grep('^band1', names(res), value = TRUE)[1]
    na_vals <- is.na(res[[first_band]])
    out[[i]] <- res[!na_vals, ]
  }
  
  # Now let’s create a tibble for easy plotting:
  hs_df <- lapply(out, as.data.frame) %>% 
    bind_rows %>% 
    as_tibble %>%
    dplyr::select(plotID, east=x,north=y, 
                  starts_with('band')) %>% 
    distinct
  hs_df   
  
  # hs_extract_pts_cloud() 
  #fuction to extract cloud contidion:cloud cover (green <10%, yellow 10%-50%, red >50%).
  source(paste0(user_wd,"/NEON-AOP/code/My_functions/hs_extract_pts_cloud.R"))
  out_cloud <- list()
  for (i in seq_along(hs_paths)) {
    res <- hs_extract_pts_cloud(hs_paths[i], pts = spdf_buffer, bands = 1:3)
    na_vals <- (res[[ "cloud_index"]]=="Fail")
    out_cloud[[i]] <- res[!na_vals, ]
  }
  
  #  create a tibble for easy plotting:
  cloud_df <- lapply(out_cloud, as.data.frame) %>% 
    bind_rows %>% 
    as_tibble %>%
    dplyr::select(plotID, east=x,north=y,cloud_index) 
  
  #check cloud condition, see if all is red
  not_red_row=nrow(cloud_df)
  cat("\n","Cloud conditions:------\n")
  cat("\n",paste0(latest_year,"-",site," cloud available plots ",not_red_row),"\n")
  cat("\n","Write cloud conditions:\n")
  write_csv(cloud_df, 
            paste0(wd,"DP3.30006.001/",latest_year,"_",unique(HARV_spc$domainID),"_",site,"_","cloud_df_windows",buffer,"m.csv"))
  cat("\n","=============\n")
  #remove cloud index not OK, green and yellow are OK, red is Fail
  hs_df_cldOK <- left_join(hs_df,cloud_df, by = c("plotID", "east","north")) %>% 
    filter(cloud_index=="OK")
  
  rm(out_cloud,out) #to save memory
  # And gather the hyperspectral columns (converting the data from wide to long form).
  long_df <- hs_df_cldOK %>%
    gather(band, reflectance, 
           -plotID,-east,-north,-cloud_index) %>%
    separate(band, c('index', 'wavelength')) %>%
    mutate(wavelength = parse_number(wavelength)) %>% 
    # filter water vapor bands out
    # filter(!between(wavelength, 1340, 1445),
    #        !between(wavelength, 1790, 1955)) %>% 
    mutate(reflectance = case_when((wavelength>1340&wavelength<1445) ~NA,
                                   (wavelength>1790&wavelength<1955)~NA,
                                   TRUE ~ reflectance)) %>% 
    group_by(plotID,cloud_index, index, wavelength) %>% 
    summarise(reflectance = mean(reflectance),
              n=n())
  long_df 
  
  write_csv(long_df, 
            paste0(wd,"DP3.30006.001/",latest_year,"_",unique(HARV_spc$domainID),"_",originalsitename,"_","HS_windows",buffer,"m.csv"))
  
  return(
    cat("\n",site,": HS succeed!-",paste0("\n","long_df saved:\n",
                                          paste0(wd,"DP3.30006.001/",latest_year,"_",
                                                 unique(HARV_spc$domainID),"_",originalsitename,"_","HS_windows",buffer,"m.csv")),
        "\n\n")
  )
  
}

processed_sites= c('TREE','CHEQ','KONA','DCFS')
#processed_sites= c('TREE','CHEQ','KONA','DCFS')------
wd=paste0(user_wd,"NEON-AOP/data/hs_lai/")
setwd(wd)
load(file='shared_flights.rda')
shared_flights

start <- Sys.time()
for (Onesite in processed_sites) {
  extract_HS_with_buffer_shared_flights(
    spc_mgp_sls_30cm_locationSOC,
    site= Onesite,
    year=0,
    buffer=3
  )
  # records=cbind(records,Onesite)
}  

# cat(records)
print( Sys.time() - start ) 

#3.processed_sites STEI-------
#STEI plots are in two UTM zone 15N,16N, there are two names for HDF5 STEI|CHEQ---------
site = c('STEI')
if(site %in% c('STEI')) {
  cat("'STEI' plots are in two UTM zone 15N,16N, there are two names for HDF5: STEI,CHEQ","\n")
}
#STEI_15N-------
#define function extract_HS_with_buffer_STEI_15N-----
extract_HS_with_buffer_STEI_15N <- function(spc_mgp_sls_30cm_locationSOC,
                                            site="STEI",
                                            year=2021,
                                            buffer=3){
  #setwd to home, then check which pc.
  setwd("~")
  whichpc=getwd()
  cat(whichpc)
  if(whichpc=="/Users/zwang61"){
    user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }else{
    user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }
  
  wd=paste0(user_wd,"NEON-AOP/data/hs_lai/")
  setwd(wd)
  
  #"'STEI' plots are in two UTM zone 15N,16N, there are two names for HDF5: STEI,CHEQ","\n"
  cat("'STEI' plots are in two UTM zone 15N,16N, there are two names for HDF5: STEI,CHEQ","\n")
  
  HARV_spc <-  spc_mgp_sls_30cm_locationSOC %>% filter(siteID == site) %>%
    arrange(rowID)
  #View(HARV_spc)
  #We want the soil plot hyperspectral data, all plots have soil(spc) measurements
  # cat(HARV_spc$utmZone)
  utmindex=HARV_spc$utmZone %in% "15N"
  east <- HARV_spc$piteasting[utmindex]
  names(east) <- HARV_spc$rowID[utmindex]
  north <- HARV_spc$pitnorthing[utmindex]
  names(north) <- HARV_spc$rowID[utmindex]
  # cat(paste0("east: ",east,"\n"))
  # cat(paste0("north: ",east,"\n"))
  utm <- "15N"
  cat(utm,"\n")
  if(length(utm)!=1){
    cat("This site ",site, " has more than one UTM zone! stop \n")
    cat(site,file="outfile_wrongUTM_files.txt",sep="\n",append=TRUE)
  }
  site_crs <- paste0("+proj=utm +zone=",parse_number(utm))
  
  
  # query the products endpoint for the product requested
  source(paste0(user_wd,"/NEON-AOP/code/My_functions/getAPI.R"))
  prod.req <- getAPI(apiURL = paste("http://data.neonscience.org/api/v0/products/", "DP3.30006.001", sep=""), 
                     token = NEON_TOKEN)
  avail <- jsonlite::fromJSON(httr::content(prod.req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)
  # error message if product not found
  if(!is.null(avail$error$status)) {
    stop(paste("No data found for product","DP3.30006.001", sep=" "))
  }
  
  # get the urls for months with data available, and subset to site
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  month.urls <- grep(site, month.urls, value = TRUE)
  
  avilb_year <- str_split(month.urls, "/") %>% 
    map_vec(9) %>% 
    substr(1,4) %>% 
    as.numeric()
  
  latest_year <- max(avilb_year)
  cat("\n",paste0("site:",site,"\navailable AOP data in year: "),avilb_year,
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
  
  dirs <- list.dirs(paste0(wd,'/DP3.30006.001'),recursive = TRUE) 
  if(utm == "15N"){
    
    dirs <- grep(paste0(latest_year,"_","CHEQ",".*Reflectance"), dirs, value = TRUE)
    hs_paths <- list.files(path=dirs,
                           pattern = paste0(".*","CHEQ",".*reflectance.h5"),
                           recursive = TRUE, full.names = TRUE)
  }else{
    dirs <- grep(paste0(latest_year,"_",site,".*Reflectance"), dirs, value = TRUE)
    hs_paths <- list.files(path=dirs, 
                           pattern = paste0(".*",site,".*reflectance.h5"), 
                           recursive = TRUE, full.names = TRUE)
  }
  
  
  #check there is error:HDF5  Can't open object-
  extents <- list()
  for(i in 1:length(hs_paths)){
    test_open_hs <- try(hs_extent(hs_paths[i]))
    if(inherits(test_open_hs, "try-error")){
      cat("cannot open this HDF5. \n")
      cat(hs_paths[i],file="outfile_wrongHDF5_files.txt",sep="\n",append=TRUE)
    }else{
      if(proj4string(CRS(hs_proj4string(hs_paths[i])))==proj4string(CRS(site_crs))){
        extents[[i]] <- test_open_hs
      }else{
        cat("crs have problem. \n")
        cat(hs_paths[i],file="outfile_wrongCRS_files.txt",sep="\n",append=TRUE)
      }
      
    }
  }
  
  east_addbuffer <- NULL
  north_addbuffer <- NULL
  if(buffer>0){
    for(k in 1:length(east)) {
      bufferEasting <- c(east[k]+1:buffer,east[k],east[k]-1:buffer)
      # names(bufferEasting) <- rep(names(east[k]),buffer+buffer+1)
      bufferNorthing <- c(north[k]+1:buffer,north[k],north[k]-1:buffer)
      # names(bufferNorthing) <- rep(names(north[k]),buffer+buffer+1)                
      buffer_pixels <- expand_grid(bufferEasting,bufferNorthing)        
      
      east_windows <- buffer_pixels$bufferEasting
      names(east_windows) <- rep(names(east[k]),length(east_windows))
      north_windows <- buffer_pixels$bufferNorthing
      names(north_windows) <- rep(names(north[k]),length(north_windows))
      
      east_addbuffer <- c(east_addbuffer,east_windows )
      north_addbuffer <- c(north_addbuffer,north_windows )
    }
  }
  
  #Now, create a SpatialPointsDataFrame of these points.
  spdf_buffer <- SpatialPointsDataFrame(coords = data.frame(x = c(east_addbuffer),
                                                            y = c(north_addbuffer)), 
                                        data = data.frame(plotID = names(east_addbuffer)),
                                        proj4string = sp::CRS(site_crs))
  
  # We can use the neonhs package to extract hyperspectral data at these locations.
  out <- list()
  for (i in seq_along(hs_paths)) {
    res <- hs_extract_pts(hs_paths[i], pts = spdf_buffer, bands = 1:426)
    first_band <- grep('^band1', names(res), value = TRUE)[1]
    na_vals <- is.na(res[[first_band]])
    out[[i]] <- res[!na_vals, ]
  }
  
  # Now let’s create a tibble for easy plotting:
  hs_df <- lapply(out, as.data.frame) %>% 
    bind_rows %>% 
    as_tibble %>%
    dplyr::select(plotID, east=x,north=y, 
                  starts_with('band')) %>% 
    distinct
  hs_df   
  
  # hs_extract_pts_cloud() 
  #fuction to extract cloud contidion:cloud cover (green <10%, yellow 10%-50%, red >50%).
  source(paste0(user_wd,"/NEON-AOP/code/My_functions/hs_extract_pts_cloud.R"))
  out_cloud <- list()
  for (i in seq_along(hs_paths)) {
    res <- hs_extract_pts_cloud(hs_paths[i], pts = spdf_buffer, bands = 1:3)
    na_vals <- (res[[ "cloud_index"]]=="Fail")
    out_cloud[[i]] <- res[!na_vals, ]
  }
  
  #  create a tibble for easy plotting:
  cloud_df <- lapply(out_cloud, as.data.frame) %>% 
    bind_rows %>% 
    as_tibble %>%
    dplyr::select(plotID, east=x,north=y,cloud_index) 
  
  #check cloud condition, see if all is red
  not_red_row=nrow(cloud_df)
  cat("\n","Cloud conditions:------\n")
  cat("\n",paste0(latest_year,"-",site," cloud available plots ",not_red_row),"\n")
  cat("\n","Write cloud conditions:\n")
  write_csv(cloud_df, 
            paste0(wd,"DP3.30006.001/",latest_year,"_",unique(HARV_spc$domainID),"_",site,"_","cloud_df_windows",buffer,"m.csv"))
  cat("\n","=============\n")
  #remove cloud index not OK, green and yellow are OK, red is Fail
  hs_df_cldOK <- left_join(hs_df,cloud_df, by = c("plotID", "east","north")) %>% 
    filter(cloud_index=="OK")
  
  rm(out_cloud,out) #to save memory
  # And gather the hyperspectral columns (converting the data from wide to long form).
  long_df <- hs_df_cldOK %>%
    gather(band, reflectance, 
           -plotID,-east,-north,-cloud_index) %>%
    separate(band, c('index', 'wavelength')) %>%
    mutate(wavelength = parse_number(wavelength)) %>% 
    # filter water vapor bands out
    # filter(!between(wavelength, 1340, 1445),
    #        !between(wavelength, 1790, 1955)) %>% 
    mutate(reflectance = case_when((wavelength>1340&wavelength<1445) ~NA,
                                   (wavelength>1790&wavelength<1955)~NA,
                                   TRUE ~ reflectance)) %>% 
    group_by(plotID,cloud_index, index, wavelength) %>% 
    summarise(reflectance = mean(reflectance),
              n=n())
  long_df 
  
  write_csv(long_df, 
            paste0(wd,"DP3.30006.001/",latest_year,"_",unique(HARV_spc$domainID),"_",site,"_15N_","HS_windows",buffer,"m.csv"))
  
  return(
    cat("\n",site,": HS succeed!-",paste0("\n","long_df saved:\n",
                                          paste0(wd,"DP3.30006.001/",latest_year,"_",
                                                 unique(HARV_spc$domainID),"_",site,"_15N_","HS_windows",buffer,"m.csv")),
        "\n\n")
  )
  
}


#STEI_16N-------
#define function extract_HS_with_buffer_STEI_16N-----
extract_HS_with_buffer_STEI_16N <- function(spc_mgp_sls_30cm_locationSOC,
                                            site="STEI",
                                            year=2021,
                                            buffer=3){
  #setwd to home, then check which pc.
  setwd("~")
  whichpc=getwd()
  cat(whichpc)
  if(whichpc=="/Users/zwang61"){
    user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }else{
    user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }
  
  wd=paste0(user_wd,"NEON-AOP/data/hs_lai/")
  setwd(wd)
  
  #"'STEI' plots are in two UTM zone 15N,16N, there are two names for HDF5: STEI,CHEQ","\n"---
  cat("'STEI' plots are in two UTM zone 15N,16N, there are two names for HDF5: STEI,CHEQ","\n")
  
  HARV_spc <-  spc_mgp_sls_30cm_locationSOC %>% filter(siteID == site) %>%
    arrange(rowID)
  #View(HARV_spc)
  #We want the soil plot hyperspectral data, all plots have soil(spc) measurements
  # cat(HARV_spc$utmZone)
  utmindex=HARV_spc$utmZone %in% "16N"
  east <- HARV_spc$piteasting[utmindex]
  names(east) <- HARV_spc$rowID[utmindex]
  north <- HARV_spc$pitnorthing[utmindex]
  names(north) <- HARV_spc$rowID[utmindex]
  # cat(paste0("east: ",east,"\n"))
  # cat(paste0("north: ",east,"\n"))
  utm <- "16N"
  cat(utm,"\n")
  if(length(utm)!=1){
    cat("This site ",site, " has more than one UTM zone! stop \n")
    cat(site,file="outfile_wrongUTM_files.txt",sep="\n",append=TRUE)
  }
  site_crs <- paste0("+proj=utm +zone=",parse_number(utm))
  
  # #Now, create a SpatialPointsDataFrame of these points.
  # spdf <- SpatialPointsDataFrame(coords = data.frame(x = c(east),
  #                                                    y = c(north)), 
  #                                data = data.frame(plotID = names(east)),
  #                                proj4string = sp::CRS(site_crs))
  # #CRS(hs_proj4string(hs_paths[1])))
  
  # query the products endpoint for the product requested
  source(paste0(user_wd,"/NEON-AOP/code/My_functions/getAPI.R"))
  prod.req <- getAPI(apiURL = paste("http://data.neonscience.org/api/v0/products/", "DP3.30006.001", sep=""), 
                     token = NEON_TOKEN)
  avail <- jsonlite::fromJSON(httr::content(prod.req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)
  # error message if product not found
  if(!is.null(avail$error$status)) {
    stop(paste("No data found for product","DP3.30006.001", sep=" "))
  }
  
  # get the urls for months with data available, and subset to site
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  month.urls <- grep(site, month.urls, value = TRUE)
  
  avilb_year <- str_split(month.urls, "/") %>% 
    map_vec(9) %>% 
    substr(1,4) %>% 
    as.numeric()
  
  latest_year <- max(avilb_year)
  cat("\n",paste0("site:",site,"\navailable AOP data in year: "),avilb_year,
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
  
  dirs <- list.dirs(paste0(wd,'/DP3.30006.001'),recursive = TRUE) 
  if(utm == "15N"){
    
    dirs <- grep(paste0(latest_year,"_","CHEQ",".*Reflectance"), dirs, value = TRUE)
    hs_paths <- list.files(path=dirs,
                           pattern = paste0(".*","CHEQ",".*reflectance.h5"),
                           recursive = TRUE, full.names = TRUE)
  }else{
    dirs <- grep(paste0(latest_year,"_",site,".*Reflectance"), dirs, value = TRUE)
    hs_paths <- list.files(path=dirs, 
                           pattern = paste0(".*",site,".*reflectance.h5"), 
                           recursive = TRUE, full.names = TRUE)
  }
  
  
  #check there is error:HDF5  Can't open object-
  extents <- list()
  for(i in 1:length(hs_paths)){
    test_open_hs <- try(hs_extent(hs_paths[i]))
    if(inherits(test_open_hs, "try-error")){
      cat("cannot open this HDF5. \n")
      cat(hs_paths[i],file="outfile_wrongHDF5_files.txt",sep="\n",append=TRUE)
    }else{
      if(proj4string(CRS(hs_proj4string(hs_paths[i])))==proj4string(CRS(site_crs))){
        extents[[i]] <- test_open_hs
      }else{
        cat("crs have problem. \n")
        cat(hs_paths[i],file="outfile_wrongCRS_files.txt",sep="\n",append=TRUE)
      }
      
    }
  }
  
  east_addbuffer <- NULL
  north_addbuffer <- NULL
  if(buffer>0){
    for(k in 1:length(east)) {
      bufferEasting <- c(east[k]+1:buffer,east[k],east[k]-1:buffer)
      # names(bufferEasting) <- rep(names(east[k]),buffer+buffer+1)
      bufferNorthing <- c(north[k]+1:buffer,north[k],north[k]-1:buffer)
      # names(bufferNorthing) <- rep(names(north[k]),buffer+buffer+1)                
      buffer_pixels <- expand_grid(bufferEasting,bufferNorthing)        
      
      east_windows <- buffer_pixels$bufferEasting
      names(east_windows) <- rep(names(east[k]),length(east_windows))
      north_windows <- buffer_pixels$bufferNorthing
      names(north_windows) <- rep(names(north[k]),length(north_windows))
      
      east_addbuffer <- c(east_addbuffer,east_windows )
      north_addbuffer <- c(north_addbuffer,north_windows )
    }
  }
  
  #Now, create a SpatialPointsDataFrame of these points.
  spdf_buffer <- SpatialPointsDataFrame(coords = data.frame(x = c(east_addbuffer),
                                                            y = c(north_addbuffer)), 
                                        data = data.frame(plotID = names(east_addbuffer)),
                                        proj4string = sp::CRS(site_crs))
  
  # We can use the neonhs package to extract hyperspectral data at these locations.
  out <- list()
  for (i in seq_along(hs_paths)) {
    res <- hs_extract_pts(hs_paths[i], pts = spdf_buffer, bands = 1:426)
    first_band <- grep('^band1', names(res), value = TRUE)[1]
    na_vals <- is.na(res[[first_band]])
    out[[i]] <- res[!na_vals, ]
  }
  
  # Now let’s create a tibble for easy plotting:
  hs_df <- lapply(out, as.data.frame) %>% 
    bind_rows %>% 
    as_tibble %>%
    dplyr::select(plotID, east=x,north=y, 
                  starts_with('band')) %>% 
    distinct
  hs_df   
  
  # hs_extract_pts_cloud() 
  #fuction to extract cloud contidion:cloud cover (green <10%, yellow 10%-50%, red >50%).
  source(paste0(user_wd,"/NEON-AOP/code/My_functions/hs_extract_pts_cloud.R"))
  out_cloud <- list()
  for (i in seq_along(hs_paths)) {
    res <- hs_extract_pts_cloud(hs_paths[i], pts = spdf_buffer, bands = 1:3)
    na_vals <- (res[[ "cloud_index"]]=="Fail")
    out_cloud[[i]] <- res[!na_vals, ]
  }
  
  #  create a tibble for easy plotting:
  cloud_df <- lapply(out_cloud, as.data.frame) %>% 
    bind_rows %>% 
    as_tibble %>%
    dplyr::select(plotID, east=x,north=y,cloud_index) 
  
  #check cloud condition, see if all is red
  not_red_row=nrow(cloud_df)
  cat("\n","Cloud conditions:------\n")
  cat("\n",paste0(latest_year,"-",site," cloud available plots ",not_red_row),"\n")
  cat("\n","Write cloud conditions:\n")
  write_csv(cloud_df, 
            paste0(wd,"DP3.30006.001/",latest_year,"_",unique(HARV_spc$domainID),"_",site,"_","cloud_df_windows",buffer,"m.csv"))
  cat("\n","=============\n")
  #remove cloud index not OK, green and yellow are OK, red is Fail
  hs_df_cldOK <- left_join(hs_df,cloud_df, by = c("plotID", "east","north")) %>% 
    filter(cloud_index=="OK")
  
  rm(out_cloud,out) #to save memory
  # And gather the hyperspectral columns (converting the data from wide to long form).
  long_df <- hs_df_cldOK %>%
    gather(band, reflectance, 
           -plotID,-east,-north,-cloud_index) %>%
    separate(band, c('index', 'wavelength')) %>%
    mutate(wavelength = parse_number(wavelength)) %>% 
    # filter water vapor bands out
    # filter(!between(wavelength, 1340, 1445),
    #        !between(wavelength, 1790, 1955)) %>% 
    mutate(reflectance = case_when((wavelength>1340&wavelength<1445) ~NA,
                                   (wavelength>1790&wavelength<1955)~NA,
                                   TRUE ~ reflectance)) %>% 
    group_by(plotID,cloud_index, index, wavelength) %>% 
    summarise(reflectance = mean(reflectance),
              n=n())
  long_df 
  
  write_csv(long_df, 
            paste0(wd,"DP3.30006.001/",latest_year,"_",unique(HARV_spc$domainID),"_",site,"_16N_","HS_windows",buffer,"m.csv"))
  
  return(
    cat("\n",site,": HS succeed!-",paste0("\n","long_df saved:\n",
                                          paste0(wd,"DP3.30006.001/",latest_year,"_",
                                                 unique(HARV_spc$domainID),"_",site,"_16N_","HS_windows",buffer,"m.csv")),
        "\n\n")
  )
  
}

processed_sites= c('STEI')
#processed_sites= c('STEI')-----
wd=paste0(user_wd,"NEON-AOP/data/hs_lai/")
setwd(wd)

start <- Sys.time()
for (Onesite in processed_sites) {
  extract_HS_with_buffer_STEI_15N(
    spc_mgp_sls_30cm_locationSOC,
    site= Onesite,
    year=0,
    buffer=3
  )
}  

for (Onesite in processed_sites) {
  extract_HS_with_buffer_STEI_16N(
    spc_mgp_sls_30cm_locationSOC,
    site= Onesite,
    year=0,
    buffer=3
  )
}  

print( Sys.time() - start ) 

#4.processed_sites c('BLAN')-------
#define function extract_HS_with_buffer_BLAN-----
extract_HS_with_buffer_BLAN <- function(spc_mgp_sls_30cm_locationSOC,
                                            site="BLAN",
                                            year=2021,
                                            buffer=3){
  #setwd to home, then check which pc.
  setwd("~")
  whichpc=getwd()
  cat(whichpc)
  if(whichpc=="/Users/zwang61"){
    user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }else{
    user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }
  
  wd=paste0(user_wd,"NEON-AOP/data/hs_lai/")
  setwd(wd)
  

  HARV_spc <-  spc_mgp_sls_30cm_locationSOC %>% filter(siteID == site) %>%
    arrange(rowID)
  #View(HARV_spc)
  #We want the soil plot hyperspectral data, all plots have soil(spc) measurements
  # cat(HARV_spc$utmZone)
  east <- HARV_spc$piteasting
  names(east) <- HARV_spc$rowID
  north <- HARV_spc$pitnorthing
  names(north) <- HARV_spc$rowID
  # cat(paste0("east: ",east,"\n"))
  # cat(paste0("north: ",east,"\n"))
  #convert easting & northing coordinates for Blandy (BLAN)
  # Blandy contains plots in 18N and plots in 17N; flight data are all in 17N
  if(site=='BLAN' & length(which(east<=250000))>0) {
    easting17 <- east[which(east>250000)]
    northing17 <- north[which(east>250000)]
    
    easting18 <- east[which(east<=250000)]
    northing18 <- north[which(east<=250000)]
    
    df18 <- cbind(easting18, northing18)
    df18 <- data.frame(df18)
    names(df18) <- c('easting','northing')
    
    sp::coordinates(df18) <- c('easting', 'northing')
    
    epsg.z <- 32617
    if(utils::packageVersion("sp")<"1.4.2") {
      sp::proj4string(df18) <- sp::CRS('+proj=utm +zone=18N ellps=WGS84')
      df18conv <- sp::spTransform(df18, sp::CRS('+proj=utm +zone=17N ellps=WGS84'))
    } else {
      raster::crs(df18) <- sp::CRS("+proj=utm +zone=18")
      df18conv <- sp::spTransform(df18, sp::CRS(paste("+init=epsg:", epsg.z, sep='')))
    }
    
    east <- c(easting17, df18conv$easting)
    north <- c(northing17, df18conv$northing)
    
    cat('Blandy (BLAN) plots include two UTM zones, flight data are all in 17N.
        Coordinates in UTM zone 18N have been converted to 17N to download the correct tiles.
        You will need to make the same conversion to connect airborne to ground data.')
  }
  
  print(paste0("sites location numbers: ",length(east)))
  length(north)
  
  utm <- "17N"
  cat(utm,"\n")
  if(length(utm)!=1){
    cat("This site ",site, " has more than one UTM zone! stop \n")
    cat(site,file="outfile_wrongUTM_files.txt",sep="\n",append=TRUE)
  }
  site_crs <- paste0("+proj=utm +zone=",parse_number(utm))
  
  # #Now, create a SpatialPointsDataFrame of these points.
  # spdf <- SpatialPointsDataFrame(coords = data.frame(x = c(east),
  #                                                    y = c(north)), 
  #                                data = data.frame(plotID = names(east)),
  #                                proj4string = sp::CRS(site_crs))
  # #CRS(hs_proj4string(hs_paths[1])))
  
  # query the products endpoint for the product requested
  source(paste0(user_wd,"/NEON-AOP/code/My_functions/getAPI.R"))
  prod.req <- getAPI(apiURL = paste("http://data.neonscience.org/api/v0/products/", "DP3.30006.001", sep=""), 
                     token = NEON_TOKEN)
  avail <- jsonlite::fromJSON(httr::content(prod.req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)
  # error message if product not found
  if(!is.null(avail$error$status)) {
    stop(paste("No data found for product","DP3.30006.001", sep=" "))
  }
  
  # get the urls for months with data available, and subset to site
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  month.urls <- grep(site, month.urls, value = TRUE)
  
  avilb_year <- str_split(month.urls, "/") %>% 
    map_vec(9) %>% 
    substr(1,4) %>% 
    as.numeric()
  
  latest_year <- max(avilb_year)
  cat("\n",paste0("site:",site,"\navailable AOP data in year: "),avilb_year,
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
  
  dirs <- list.dirs(paste0(wd,'/DP3.30006.001'),recursive = TRUE) 
  if(utm == "15N"){
    
    dirs <- grep(paste0(latest_year,"_","CHEQ",".*Reflectance"), dirs, value = TRUE)
    hs_paths <- list.files(path=dirs,
                           pattern = paste0(".*","CHEQ",".*reflectance.h5"),
                           recursive = TRUE, full.names = TRUE)
  }else{
    dirs <- grep(paste0(latest_year,"_",site,".*Reflectance"), dirs, value = TRUE)
    hs_paths <- list.files(path=dirs, 
                           pattern = paste0(".*",site,".*reflectance.h5"), 
                           recursive = TRUE, full.names = TRUE)
  }
  
  
  #check there is error:HDF5  Can't open object-
  extents <- list()
  for(i in 1:length(hs_paths)){
    test_open_hs <- try(hs_extent(hs_paths[i]))
    if(inherits(test_open_hs, "try-error")){
      cat("cannot open this HDF5. \n")
      cat(hs_paths[i],file="outfile_wrongHDF5_files.txt",sep="\n",append=TRUE)
    }else{
      if(proj4string(CRS(hs_proj4string(hs_paths[i])))==proj4string(CRS(site_crs))){
        extents[[i]] <- test_open_hs
      }else{
        cat("crs have problem. \n")
        cat(hs_paths[i],file="outfile_wrongCRS_files.txt",sep="\n",append=TRUE)
      }
      
    }
  }
  
  east_addbuffer <- NULL
  north_addbuffer <- NULL
  if(buffer>0){
    for(k in 1:length(east)) {
      bufferEasting <- c(east[k]+1:buffer,east[k],east[k]-1:buffer)
      # names(bufferEasting) <- rep(names(east[k]),buffer+buffer+1)
      bufferNorthing <- c(north[k]+1:buffer,north[k],north[k]-1:buffer)
      # names(bufferNorthing) <- rep(names(north[k]),buffer+buffer+1)                
      buffer_pixels <- expand_grid(bufferEasting,bufferNorthing)        
      
      east_windows <- buffer_pixels$bufferEasting
      names(east_windows) <- rep(names(east[k]),length(east_windows))
      north_windows <- buffer_pixels$bufferNorthing
      names(north_windows) <- rep(names(north[k]),length(north_windows))
      
      east_addbuffer <- c(east_addbuffer,east_windows )
      north_addbuffer <- c(north_addbuffer,north_windows )
    }
  }
  
  #Now, create a SpatialPointsDataFrame of these points.
  spdf_buffer <- SpatialPointsDataFrame(coords = data.frame(x = c(east_addbuffer),
                                                            y = c(north_addbuffer)), 
                                        data = data.frame(plotID = names(east_addbuffer)),
                                        proj4string = sp::CRS(site_crs))
  
  # We can use the neonhs package to extract hyperspectral data at these locations.
  out <- list()
  for (i in seq_along(hs_paths)) {
    res <- hs_extract_pts(hs_paths[i], pts = spdf_buffer, bands = 1:426)
    first_band <- grep('^band1', names(res), value = TRUE)[1]
    na_vals <- is.na(res[[first_band]])
    out[[i]] <- res[!na_vals, ]
  }
  
  # Now let’s create a tibble for easy plotting:
  hs_df <- lapply(out, as.data.frame) %>% 
    bind_rows %>% 
    as_tibble %>%
    dplyr::select(plotID, east=x,north=y, 
                  starts_with('band')) %>% 
    distinct
  hs_df   
  
  # hs_extract_pts_cloud() 
  #fuction to extract cloud contidion:cloud cover (green <10%, yellow 10%-50%, red >50%).
  source(paste0(user_wd,"/NEON-AOP/code/My_functions/hs_extract_pts_cloud.R"))
  out_cloud <- list()
  for (i in seq_along(hs_paths)) {
    res <- hs_extract_pts_cloud(hs_paths[i], pts = spdf_buffer, bands = 1:3)
    na_vals <- (res[[ "cloud_index"]]=="Fail")
    out_cloud[[i]] <- res[!na_vals, ]
  }
  
  #  create a tibble for easy plotting:
  cloud_df <- lapply(out_cloud, as.data.frame) %>% 
    bind_rows %>% 
    as_tibble %>%
    dplyr::select(plotID, east=x,north=y,cloud_index) 
  
  #check cloud condition, see if all is red
  not_red_row=nrow(cloud_df)
  cat("\n","Cloud conditions:------\n")
  cat("\n",paste0(latest_year,"-",site," cloud available plots ",not_red_row),"\n")
  cat("\n","Write cloud conditions:\n")
  write_csv(cloud_df, 
            paste0(wd,"DP3.30006.001/",latest_year,"_",unique(HARV_spc$domainID),"_",site,"_","cloud_df_windows",buffer,"m.csv"))
  cat("\n","=============\n")
  #remove cloud index not OK, green and yellow are OK, red is Fail
  hs_df_cldOK <- left_join(hs_df,cloud_df, by = c("plotID", "east","north")) %>% 
    filter(cloud_index=="OK")
  
  rm(out_cloud,out) #to save memory
  # And gather the hyperspectral columns (converting the data from wide to long form).
  long_df <- hs_df_cldOK %>%
    gather(band, reflectance, 
           -plotID,-east,-north,-cloud_index) %>%
    separate(band, c('index', 'wavelength')) %>%
    mutate(wavelength = parse_number(wavelength)) %>% 
    # filter water vapor bands out
    # filter(!between(wavelength, 1340, 1445),
    #        !between(wavelength, 1790, 1955)) %>% 
    mutate(reflectance = case_when((wavelength>1340&wavelength<1445) ~NA,
                                   (wavelength>1790&wavelength<1955)~NA,
                                   TRUE ~ reflectance)) %>% 
    group_by(plotID,cloud_index, index, wavelength) %>% 
    summarise(reflectance = mean(reflectance),
              n=n())
  long_df 
  
  write_csv(long_df, 
            paste0(wd,"DP3.30006.001/",latest_year,"_",unique(HARV_spc$domainID),"_",site,"_","HS_windows",buffer,"m.csv"))
  
  return(
    cat("\n",site,": HS succeed!-",paste0("\n","long_df saved:\n",
                                          paste0(wd,"DP3.30006.001/",latest_year,"_",
                                                 unique(HARV_spc$domainID),"_",site,"_","HS_windows",buffer,"m.csv")),
        "\n\n")
  )
  
}
#processed_sites= c('BLAN')--------
processed_sites= c('BLAN')

wd=paste0(user_wd,"NEON-AOP/data/hs_lai/")
setwd(wd)

start <- Sys.time()
for (Onesite in processed_sites) {
  extract_HS_with_buffer_BLAN(
    spc_mgp_sls_30cm_locationSOC,
    site= Onesite,
    year=0,
    buffer=3
  )
  
}