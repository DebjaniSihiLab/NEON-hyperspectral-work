#Extract hyperspectral data at site including all plots center with windows 3m*3m  

#install.packages('devtools')
# devtools::install_github('earthlab/neonhs')
library(neonUtilities)
NEON_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ6aHVvbmFuLndhbmdAZW1vcnkuZWR1Iiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxNzg3Njk4ODU4LCJpYXQiOjE2MzAwMTg4NTgsImVtYWlsIjoiemh1b25hbi53YW5nQGVtb3J5LmVkdSJ9.Ug3DuSLWLT0NsZy5pHKs8Wh51C9-tjxoCccbhrUW0m49n1GikPp0EQ3jZc3c6FFzRpDk04UXA-k4fECyxzqnmQ"
library(geoNEON)
library(neonhs)
library(rhdf5)
library(tidyverse)
library(terra)
library(raster)
library(rgdal)
library(sf)


hs_extract_PlotsSites_buffer <- function(wd ="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/hs_lai/",
                                         spc_mgp_sls_30cm_locationSOC,
                                         site="HARV",buffer=2,
                                         downloadAOPfile=FALSE,
                                         year=2021,
                                         whichpc="personal_pc") {

  if(whichpc=="/Users/zwang61"){
    user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }else{
    user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }
  
# we want to save our files. Be sure to move the download into your working directory!
setwd(wd)
cat("DP3.30006.001, hyperspectral data: \n")
#Tutorial:
#Extract spectra at spatial point locations
#https://www.earthdatascience.org/neonhs/index.html
#Extracting hyperspectral data for plant species in NEON ground plots
#https://www.earthdatascience.org/neonhs/articles/extracting-hyperspectral-data-for-plant-species-in-neon-ground-plots.html

#Data preparation
#https://jesusnpl.github.io/NEON_Spectra_2021.html
#download the Terrestrial Observation System Sampling Locations dataset------
# The first step is to prepare the information required to download the hyperspectral data from NEON-AOP.
# To do that we will first download the Terrestrial Observation System Sampling Locations dataset.
# You can download it from NEON Spatial Data & Maps, 
#https://www.neonscience.org/data-samples/data/spatial-data-maps
# specifically under the Terrestrial Observation System Sampling Locations tab
#explain sites, plots details:
#https://www.neonscience.org/data-samples/data-collection/observational-sampling/site-level-sampling-design
#select site:------------------
# site="HARV" #This is siteID
#line 168 set buffer----------
# buffer <- 5
# NEON_plots <- readOGR(dsn = "/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/All_NEON_TOS_Plots_V8", 
#                       layer = "All_NEON_TOS_Plot_Polygons_V8")
# #look into D01 HARV harvard forest
# HARV_plots <- NEON_plots@data %>% 
#   filter(siteID == "HARV" & subtype == "basePlot") %>% 
#   #filter(siteID == "HARV" & plotType == "distributed" & subtype == "basePlot") %>% 
#   arrange(plotID)
# View(HARV_plots)

#spc:Soil physical and chemical properties, distributed initial characterization DP1.10047.001
# HARV_spc <- read.csv(paste("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NSF-SOC/NSF-MSA-SOC/neon_data/compiled/spatial/spatial_spc.csv", sep = "/"))%>% 
#   filter(siteID == site) %>% 
#   arrange(plotID)
# View(HARV_spc)

# HARV_spc <- spc_mgp_sls_30cm_locationSOC <- read_csv("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/hs_lai/NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC.csv")
# spc_mgp_sls_30cm_locationSOC$rowID=1:nrow(spc_mgp_sls_30cm_locationSOC)

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
unique(HARV_spc$utmZone)

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

#year, which year is the best, hwo to decide the year
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
# Download hyperspectral imagery from NEON-AOP-----------------------------
# Now we have the UTM coordinates from the plot HARV_001 we can download 
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
cat("Parameter downloadAOPfile =", downloadAOPfile,"\n\n")
if(downloadAOPfile==TRUE){
  #warning during download:
  # If file sizes are large, increase the timeout limit on your machine: 
  options(timeout=60*120)
  getOption('timeout')
  
  #one site by one site, otherwise may meet error could not be downloaded
  #set  check.size = F, so continously download
  # for (s in 1:length(north)) {
  #   byTileAOP(dpID = "DP3.30006.001", # NEON-AOP product
  #             site = site, # Site code
  #             year = latest_year, # Year
  #             check.size = F, #check.size = F, so continously download
  #             easting = east[s], northing = north[s], # Coordinates UTM
  #             buffer=20,#with buffer could include more tiles
  #             savepath = "/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/hs_lai", # Path
  #             token = NEON_TOKEN) 
  #   cat("successful download site number: ",s,"\n")
  # }
  byTileAOP(dpID = "DP3.30006.001", # NEON-AOP product
            site = site, # Site code
            year = latest_year, # Year
            check.size = F,
            easting = east, northing = north, # Coordinates UTM
            buffer=20,#with buffer could include more tiles
            savepath = paste0(user_wd,"/NEON-AOP/data/hs_lai/"), # Path
            token = NEON_TOKEN)
}else{
  cat("\n","do not download this site HDF5 data\n")
}

load(file='shared_flights.rda')
# if(site=="STEI"){
#   #site STEI has two folder STEI and CHEQ
#   dirs <- list.dirs(paste0(wd,'/DP3.30006.001'),recursive = TRUE) 
#   dirs <- grep(paste0(latest_year,"_","[STEI|CHEQ]",".*Reflectance"), dirs, value = TRUE)
#   hs_paths <- list.files(path=dirs, 
#                          pattern = paste0(".*","[STEI|CHEQ]",".*reflectance.h5"), 
#                          recursive = TRUE, full.names = TRUE)
#   }else if(site=="TREE"){
#     dirs <- list.dirs(paste0(wd,'/DP3.30006.001'),recursive = TRUE) 
#     dirs <- grep(paste0(latest_year,"_","STEI",".*Reflectance"), dirs, value = TRUE)
#     hs_paths <- list.files(path=dirs, 
#                            pattern = paste0(".*","STEI",".*reflectance.h5"), 
#                            recursive = TRUE, full.names = TRUE)
# }
dirs <- list.dirs(paste0(wd,'/DP3.30006.001'),recursive = TRUE) 
dirs <- grep(paste0(latest_year,"_",site,".*Reflectance"), dirs, value = TRUE)
hs_paths <- list.files(path=dirs, 
                      pattern = paste0(".*",site,".*reflectance.h5"), 
                      recursive = TRUE, full.names = TRUE)

#Now, create a SpatialPointsDataFrame of these points.
spdf <- SpatialPointsDataFrame(coords = data.frame(x = c(east),
                                                   y = c(north)), 
                               data = data.frame(plotID = names(east)),
                               proj4string = CRS(hs_proj4string(hs_paths[1])))
#wait 60 seconds after download-----
Sys.sleep(60)
#Now, let’s visualize the extents of these hyperspectral images and the locations of the mapped plants:
#check there is error:HDF5  Can't open object----
extents <- try(lapply(hs_paths, hs_extent))
if(inherits(extents, "try-error")){
  #error handling code, maybe just skip this iteration using
  s1=paste0(latest_year," ",site," site: HDF5 open failed. Ignore this site, move to next site.\n")
  cat(s1)
  s2=paste0("site:",site," available AOP data in year: ", toString(avilb_year),".\n")
  cat(s2)
  write_csv(data.frame(s1,s2), 
            paste0(wd,"DP3.30006.001/",latest_year,"_",unique(HARV_spc$domainID),"_",site,"_","HS_windows",buffer,"m.csv"))

}else{
#rest of iteration for case of no error

# extents <- lapply(hs_paths, hs_extent) #move to line 214, try.
# plot(do.call(raster::merge, extents), bty = 'n', 
#      xlab = 'Easting', ylab = 'Northing')
# plot(spdf, add = TRUE)
# for (e in extents) {
#   plot(e, add = TRUE)
# }

#add buffer to extract the pixels around centroid/spdf
#buffer is 10m or 20m
#east；north add the windows pixels around output 
#output: east_addbuffer; north_addbuffer
# apply buffer,add pixels around plot centroid---------
# buffer <- 5
# buffer <- 10
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
                               proj4string = CRS(hs_proj4string(hs_paths[1])))

#Now, let’s visualize the extents of these hyperspectral images and the locations of the mapped plants:
# extents <- lapply(hs_paths, hs_extent)
# plot(do.call(raster::merge, extents), bty = 'n', 
#      xlab = 'Easting', ylab = 'Northing')
# plot(spdf_buffer, add = TRUE)
# for (e in extents) {
#   plot(e, add = TRUE)
# }

# Each mapped plant location is covered by an extent object for a hyperspectral image. 
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
 
 cloud_df
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

}

get_Hs_available_year <- function(site,whichpc){
  
  if(whichpc=="/Users/zwang61"){
    user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }else{
    user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }
  
  source(paste0(user_wd,"/NEON-AOP/code/My_functions/getAPI.R"))
  prod.req <- getAPI(apiURL = paste("http://data.neonscience.org/api/v0/products/", "DP3.30012.001", sep=""), 
                     token = NEON_TOKEN)
  avail <- jsonlite::fromJSON(httr::content(prod.req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)
  # error message if product not found
  if(!is.null(avail$error$status)) {
    stop(paste("No data found for product","DP3.30012.001", sep=" "))
  }
  
  # get the urls for months with data available, and subset to site
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  month.urls <- grep(site, month.urls, value = TRUE)
  
  avilb_year <- str_split(month.urls, "/") %>% 
    map_vec(9) %>% 
    substr(1,4) %>% 
    as.numeric()
  return(avilb_year)
}

  