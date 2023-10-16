#Extract LAI data at site including all plots center with buffer>= 1m 

#based on Poster:/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/reference
#"Opportunistic Validation of Vegetation Indices from MODIS and VIIRS Using NEON AOP Hyperspectral Imagery"
#select site location information
# 3 sites: different ecosystem, LAI and SOC should be different:
# D01 HARV harvard forest 
# D14 SRER desert southwest
# D09 WOOD northern plains

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

lai_extract_PlotsSites_buffer <- function(wd = "/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/hs_lai/",
                                          spc_mgp_sls_30cm_locationSOC,
                                          site="HARV",buffer=1,
                                          downloadAOPfile=FALSE,
                                          year=2021,
                                          whichpc="personal_pc"){
  if(whichpc=="/Users/zwang61"){
    user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }else{
    user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }
  
# we want to save our files. Be sure to move the download into your working directory!
#"/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/hs_lai/" #This will depend on your local environment
setwd(wd)

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
# # View(HARV_spc)
# 
# #We want the soil plot hyperspectral data, all plots have soil(spc) measurements
# cat(HARV_spc$utmZone)
# east <- HARV_spc$easting
# names(east) <- HARV_spc$plotID
# north <- HARV_spc$northing
# names(north) <- HARV_spc$plotID
# cat(paste0("east: ",east,"\n"))
# cat(paste0("north: ",east,"\n"))
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

#year, which year is the best, hwo to decide the year
# query the products endpoint for the product requested
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

latest_year <- max(avilb_year)
cat(paste0("site:",site,"\navailable LAI data in year: "),avilb_year,
    "\nlatest year: ",latest_year,"\n")
if(year==0){
  cat("didn't specify year, use  available latest_year")
}else{
  if(!year %in% avilb_year){
    cat("user choose year is not in available year at this site.----->\n",
        "use  available latest_year",latest_year,"\n")
  }else{
    latest_year <- year
    cat("download user choose year, latest_year=", year)
  }
}

# Download LAI imagery from NEON-AOP-----------------------------
# Now we have the UTM coordinates from the plot HARV_001 we can download 
#.tif format

#
dir <- paste0(wd,"/DP3.30012.001/neon-aop-products/",latest_year,"/FullSite/",unique(HARV_spc$domainID))
dirs <- list.dirs(dir)
sites_dirs <- grep(paste0(latest_year,"_",site), dirs)
length(sites_dirs)#if there is no this sites file, length==0,then download them.
if (length(sites_dirs)==0){
  cat("this site LAI tiff data doesn't exists\n")
}else{
  cat("this site LAI tiff data exists\n")
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
  #   byTileAOP(dpID = "DP3.30012.001", # NEON-LAI product
  #             site = site, # Site code
  #             year = latest_year, # Year
  #             check.size = F, #check.size = F, so continously download
  #             easting = east[s], northing = north[s], # Coordinates UTM
  #             buffer=20,#with buffer could include more tiles
  #             savepath = "/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/hs_lai", # Path
  #             token = NEON_TOKEN) 
  #   cat("successful download site number: ",s,"\n")
  # }
  byTileAOP(dpID = "DP3.30012.001", # NEON-AOP product
            site = site, # Site code
            year = latest_year, # Year
            check.size = F,
            easting = east, northing = north, # Coordinates UTM
            buffer=20,#with buffer could include more tiles
            savepath = paste0(user_wd,"NEON-AOP/data/hs_lai"), # Path
            token = NEON_TOKEN)
}else{
  cat("not download this site LAI tif data\n")
}
#warning during download:
# If file sizes are large, increase the timeout limit on your machine: 
#options(timeout=200)
#getOption('timeout')
dirs <- list.dirs(paste0(wd,'/DP3.30012.001'),recursive = TRUE) 
dirs <- grep(paste0(latest_year,"_",site,".*LAI"), dirs, value = TRUE)
hs_paths <- list.files(path=dirs, 
                      pattern = paste0(".*",site,".*LAI.tif"), 
                      recursive = TRUE, full.names = TRUE)

epsg=crs(rast(hs_paths[1]), describe=TRUE, proj=TRUE) [["code"]]
#Now, create a SpatialPointsDataFrame of these points.

spdf <- SpatialPointsDataFrame(coords = data.frame(x = c(east),
                                                   y = c(north)), 
                               data = data.frame(plotID = names(east)),
                               proj4string = CRS(paste0("+init=epsg:", epsg)))
                                                 
#Now, let’s visualize the extents of these hyperspectral images and the locations of the mapped plants:
extents <- hs_paths %>%  
  map(rast) %>% 
  map(ext) 
  
plot(reduce(extents,terra::union), bty = 'n', 
     xlab = 'Easting', ylab = 'Northing')
plot(spdf, add = TRUE)
for (e in extents) {
  plot(e, add = TRUE)
}

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
                               proj4string = CRS(paste0("+init=epsg:", epsg)))

#Now, let’s visualize the extents of these hyperspectral images and the locations of the mapped plants:
extents <- hs_paths %>%  
  map(rast) %>% 
  map(ext) 

plot(reduce(extents,terra::union), bty = 'n', 
     xlab = 'Easting', ylab = 'Northing')
plot(spdf_buffer, add = TRUE)
for (e in extents) {
  plot(e, add = TRUE)
}


#  extract lai tif data at these locations.
 out <- list()
 for (i in seq_along(hs_paths)) {
   r=rast(hs_paths[i])
   points <- vect(spdf_buffer)
   res <- terra::extract(r, points, xy=TRUE,ID=TRUE,bind=TRUE)
   names(res)[2]="LAI"
   out[[i]] <- res[!is.na(res$LAI), ]
 }

# Now let’s create a tibble for easy plotting:
 hs_df <- lapply(out, as.data.frame) %>% 
   bind_rows %>% 
   as_tibble %>%
   dplyr::select(plotID, LAI, east=x,north=y) %>% 
   distinct
 hs_df   
 

# And gather the hyperspectral columns (converting the data from wide to long form).
 long_df <- hs_df %>% 
   group_by(plotID) %>% 
   summarise(LAI = mean(LAI),
             n=n())
long_df 

write_csv(long_df, 
          paste0(wd,"DP3.30012.001/",latest_year,"_",unique(HARV_spc$domainID),"_",site,"_","LAI_windows",buffer,"m.csv"))

return(cat(site,": LAI succeed!-",paste0("long_df saved:\n",
          paste0(wd,"DP3.30012.001/",latest_year,"_",unique(HARV_spc$domainID),"_",site,"_","LAI_windows",buffer,"m.csv"))
      ))
}



  




