library("rgdal")
library(sp)
library(sf)
library(terra)
library(tidyverse)
library(raster)
library(doParallel)
NEON_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ6aHVvbmFuLndhbmdAZW1vcnkuZWR1Iiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxNzg3Njk4ODU4LCJpYXQiOjE2MzAwMTg4NTgsImVtYWlsIjoiemh1b25hbi53YW5nQGVtb3J5LmVkdSJ9.Ug3DuSLWLT0NsZy5pHKs8Wh51C9-tjxoCccbhrUW0m49n1GikPp0EQ3jZc3c6FFzRpDk04UXA-k4fECyxzqnmQ"

extract_HS_with_buffer <- function(spc_mgp_sls_30cm_locationSOC,
                                   site="TOOL",
                                   year=2021,
                                   buffer=3){
  user_wd="/lustre/or-scratch/cades-ccsi/scratch/z2w/NEON_hs/"
  wd="/lustre/or-scratch/cades-ccsi/scratch/z2w/NEON_hs/data/"
  setwd(wd)
  cat("BRDF_topo_corrected ENVI data:---DP1.30006.001 NEON-AOP flight line. \n Site:",site,"\n")  
  
  load(file='shared_flights.rda')
  shared_flights <- shared_flights[-2,]
  
  if(site %in% shared_flights$site){
    flightsite=shared_flights$flightSite[which(site==shared_flights$site)]
  }else{
    flightsite=site
    }
    
  HARV_spc <-  spc_mgp_sls_30cm_locationSOC %>% filter(siteID == flightsite) %>%
    arrange(rowID)
 
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
    
    HARV_spc$utmZone <- "17N"
    
  }
  
  utm <- unique(HARV_spc$utmZone)
  cat(utm,"\n")
  
  if(length(utm)!=1){
    cat("This site ",site, " has more than one UTM zone! stop \n")
    cat(site,file="outfile_wrongUTM_files.txt",sep="\n",append=TRUE)
  }
  
 
  
  site_crs <- paste0("+proj=utm +zone=",parse_number(utm))
  
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
      cat(" user choose year, latest_year=", year,"\n")
    }
  }
  
  if(site=="CHEQ"){
    latest_year <- year
  }

  dir <- paste0(wd,"/DP1.30006.001/neon-aop-products/",latest_year,"/FullSite/",unique(HARV_spc$domainID))
  dirs <- list.dirs(dir)
  sites_dirs <- grep(paste0(latest_year,"_",flightsite), dirs,value =TRUE)
  files_corrected=list.files(sites_dirs, pattern = "reflectance_BRDF_topo_corrected$",full.names = TRUE)

  
  #  extract lai tif data at these locations.
  out <- list()
  for (i in seq_along(files_corrected)) {
    r=rast(files_corrected[i])
    points <- terra::vect(spdf_buffer)
    res <- terra::extract(r, points, xy=TRUE,ID=TRUE,bind=TRUE)
    #on CADES HPC, terra::extract return data.frame with ID. wired!
    data = data.frame(plotID = names(east_addbuffer))
    res <- cbind(res,data)
    
    hs_df <-  as_tibble(res) %>% 
      drop_na()
    out[[i]] <- hs_df
  }
  
  hs_df_wide <- out %>% 
    bind_rows %>% 
    as_tibble %>%
    dplyr::select(plotID, east=x,north=y, ends_with('nanometers'))
  # %>% 
  #   distinct
  
  write_csv(hs_df_wide, 
            paste0(wd,"DP1.30006.001/",
                   unique(HARV_spc$domainID),"_",site,"_",latest_year,"_","HS_buf_",buffer,"m_wide.csv")
  )
  
  
  hs_df_long = hs_df_wide%>%
    distinct %>% 
    gather(band, reflectance, 
           -plotID,-east,-north) %>% 
  mutate(wavelength = parse_number(band))%>% 
    # mutate(reflectance = case_when((wavelength>1340&wavelength<1445) ~NA,
    #                                (wavelength>1790&wavelength<1955)~NA,
    #                                TRUE ~ reflectance)) %>% 
    mutate(reflectance = ifelse(wavelength>1340&wavelength<1445, NA,reflectance)) %>%
    mutate(reflectance = ifelse(wavelength>1790&wavelength<1955, NA,reflectance)) %>%
    group_by(plotID, wavelength) %>%
    summarise(reflectance = mean(reflectance),
              n=n())
  
  
write_csv(hs_df_long, 
            paste0(wd,"DP1.30006.001/",
                   unique(HARV_spc$domainID),"_",site,"_",latest_year,"_","HS_buf_",buffer,"m_long.csv")
          )
  

}
#==============================================================
user_wd="/lustre/or-scratch/cades-ccsi/scratch/z2w/NEON_hs/"
spc_mgp_sls_30cm_locationSOC <- read_csv(paste0(user_wd,"spc_mgp_sls_30cm_locationSOC_withPFT_Soiltype.csv"))
spc_mgp_sls_30cm_locationSOC$rowID=1:nrow(spc_mgp_sls_30cm_locationSOC)
allsites=unique(spc_mgp_sls_30cm_locationSOC$siteID)
length(allsites) #47 we have all NEON has 47 terrestrial sites + CHEQ total 48
allsites
validsites <- allsites[!allsites%in% c("LAJA","ORNL")]
neonsites <- read_csv(paste0(user_wd,"neon_hs_sites.csv"))
neonsites$site %>% length()


# for(a_site in validsites){
#   siteyear=neonsites[neonsites$site==a_site,2][[1]]
#   #run function:
#   extract_HS_with_buffer(spc_mgp_sls_30cm_locationSOC,site=a_site,
#                          year=siteyear,buffer=3)
# }

#create the cluster
my.cluster <- parallel::makeCluster(
  32, 
  type = "FORK"
)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()
foreach::getDoParWorkers()
start <- Sys.time()

result <- foreach(a_site = validsites, .packages=c("tidyverse","terra","sp","rgdal","raster"), .combine = 'c') %dopar% {
  siteyear=neonsites[neonsites$site==a_site,2][[1]]
  #run function:
  NEON_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ6aHVvbmFuLndhbmdAZW1vcnkuZWR1Iiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxNzg3Njk4ODU4LCJpYXQiOjE2MzAwMTg4NTgsImVtYWlsIjoiemh1b25hbi53YW5nQGVtb3J5LmVkdSJ9.Ug3DuSLWLT0NsZy5pHKs8Wh51C9-tjxoCccbhrUW0m49n1GikPp0EQ3jZc3c6FFzRpDk04UXA-k4fECyxzqnmQ"
  extract_HS_with_buffer(spc_mgp_sls_30cm_locationSOC,site=a_site,
                         year=siteyear,buffer=3)
}

print( Sys.time() - start ) 
parallel::stopCluster(cl = my.cluster)

